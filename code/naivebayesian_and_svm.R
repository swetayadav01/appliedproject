##required packages
require(tidyr)
require(tidytext)
require(tibble)
require(SnowballC)
require(stats)
require(dplyr)
require(stringr)

#required packages for svm
require(kernlab)
require(e1071)

#required packages for naive-bayesian model
require(naivebayes)
require(wordcloud)


#loading the unprocessed dataset
trv <- data.frame(read.csv("C:/Users/Sweta/Desktop/Final Project/Data/training_variants"))
tev <- data.frame(read.csv("C:/Users/Sweta/Desktop/Final Project/Data/test_variants"))

temp <- readLines("C:/Users/Sweta/Desktop/Final Project/Data/training_text")
temp <- str_split_fixed(temp[2:length(temp)], "\\|\\|",2)
trxt <- data_frame(ID=temp[,1], text=temp[,2])

temp <- readLines("C:/Users/Sweta/Desktop/Final Project/Data/test_text")
temp <- str_split_fixed(temp[2:length(temp)], "\\|\\|",2)
text <- data_frame(ID=temp[,1], text=temp[,2])

#loading the processed data
tr_feature <- data.frame(read.csv("C:/Users/Sweta/Desktop/Final Project/Data/tr_feature.csv"))
te_feature <- data.frame(read.csv("C:/Users/Sweta/Desktop/Final Project/Data/te_feature.csv"))
class_word <- data.frame(read.csv("C:/Users/Sweta/Desktop/Final Project/Data/class_word.csv"))
class_word_tf <- data.frame(read.csv("C:/Users/Sweta/Desktop/Final Project/Data/class_word_tf.csv"))
class_bigram <- data.frame(read.csv("C:/Users/Sweta/Desktop/Final Project/Data/class_bigram.csv"))
class_bigram_tf <- data.frame(read.csv("C:/Users/Sweta/Desktop/Final Project/Data/class_bigram_tf.csv"))



#data processing for training the model
{
  data("stop_words")
  top_word <- function(x, y){
    temp <- x %>% 
      unnest_tokens(word, text, to_lower=TRUE) %>%
      mutate(word=wordStem(word)) %>%
      group_by(word) %>%
      count() %>%
      arrange(desc(n)) %>%
      head(n=y) %>%
      select(word, n)
    
    return(temp)
  }
  top_20_word <- top_word(trxt, 20) 
  
  ## Creating word tokens
  tr_word_token <- trxt %>% 
    unnest_tokens(word, text) %>%
    mutate(word=wordStem(word)) %>%
    count(ID, word) %>% 
    merge(trv, by="ID") %>%
    select(ID, word, n, Class)
  
  tr_word_token <-  tr_word_token %>% 
    filter(!word %in% top_20_word$word) %>%
    filter(!word %in% stop_words$word)
  
  word_filter <- tr_word_token %>%
    bind_tf_idf(word, ID, n) %>%
    select(word, tf_idf) %>%
    unique() %>%
    arrange(tf_idf) %>% 
    select(word) %>%
    unique() %>%
    head(n=30)
  
  tr_word_token <-  tr_word_token %>% 
    filter(!word %in% word_filter$word) 
  
  #Discarding stop words top 20 words
  
  te_word_token <- text  %>%
    unnest_tokens(word, text) %>%
    mutate(word=wordStem(word)) %>%
    count(ID, word) %>%
    filter(!word %in% top_20_word$word) %>%
    filter(!word %in% stop_words$word) %>% 
    filter(!word %in% word_filter$word) %>%
    merge(tev, by="ID") %>%
    select(ID, word, n)
  
  
  ##Creating bigram tokens
  tr_bigram_token <- trxt %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c('w1','w2'), sep=" ") %>%
    mutate(w1=wordStem(w1)) %>%
    mutate(w2=wordStem(w2)) %>%
    filter(!w1 %in% stop_words$word) %>%
    filter(!w2 %in% stop_words$word) %>%
    filter(!w1 %in% top_20_word$word) %>%
    filter(!w2 %in% top_20_word$word) %>%
    unite(bigram, w1, w2, sep=" ") %>%
    count(ID, bigram) %>% 
    merge(trv, by="ID") %>%
    select(ID, bigram, n, Class)
  
  bigram_filter <- tr_bigram_token %>%
    bind_tf_idf(bigram, ID, n) %>%
    select(bigram, tf_idf) %>%
    unique() %>%
    arrange(tf_idf) %>% 
    select(bigram) %>%
    unique() %>%
    head(n=15)
  
  tr_bigram_token <- tr_bigram_token %>%
    filter(!bigram %in% bigram_filter$bigram)
  
  te_bigram_token <- text %>%
    unnest_tokens(bigram, text, token="ngrams", n=2) %>%
    separate(bigram, c('w1','w2'), sep=" ") %>%
    mutate(w1=wordStem(w1)) %>%
    mutate(w2=wordStem(w2)) %>%
    filter(!w1 %in% stop_words$word) %>%
    filter(!w2 %in% stop_words$word) %>%
    filter(!w1 %in% top_20_word$word) %>%
    filter(!w2 %in% top_20_word$word) %>%
    unite(bigram, w1, w2, sep=" ") %>%
    filter(!bigram %in% bigram_filter$bigram) %>%
    count(ID, bigram) %>% 
    merge(tev, by="ID") %>%
    select(ID, bigram, n) 
  
  head(class_word)
  head(tr_word_token)
  head(te_word_token)
  head(class_bigram)
  head(tr_bigram_token)
  head(te_bigram_token)
}

nrow(te_bigram_token)

# Creating a frequency table of each ID with given variables

freq_table <- function(feature=x, data=y, by=z, token=w, i=i, pur=k){
  
  feature <- feature %>% 
    filter(Class==i)
  
  if(pur=="train"){
    data <- data %>% 
      filter(Class==i) 
  }
  
  if(token=="word"){
    if(by=="tf_idf"){
      feature <- feature %>%
        mutate(n=tf_idf) %>%
        select(-tf_idf)
      
      data <- data %>%
        bind_tf_idf(word, ID, n) %>%
        select(-n, -tf, -idf) %>%
        mutate(n=tf_idf) %>%
        select(-tf_idf)
    }
    
    crs_join <- merge(unique(data %>% select(ID)), feature$word, by=NULL) %>%
      mutate(word=y) %>%
      select(-y) %>%
      arrange(as.numeric(ID))
    
    ft_vec <- as.character(unique(feature$word))
    
    data <- data %>%
      filter(word %in% ft_vec)
    
    if(pur=="train"){
      data <- data %>%
        select(-Class) }
    
    lft_join <- merge(crs_join, data, all.x="TRUE") %>%
      arrange(as.numeric(ID))
    lft_join[is.na(lft_join)] <- 0
    lft_join <- lft_join %>% unique() 
    
    tab <- dcast(lft_join, ID~word, value.var="n", fill=0) %>%
      arrange(as.numeric(ID))
  } 
  if(token=="bigram"){
    if(by=="tf_idf"){
      feature <- feature %>%
        mutate(n=tf_idf) %>%
        select(-tf_idf)
      
      data <- data %>%
        bind_tf_idf(bigram, ID, n) %>%
        select(-n, -tf, -idf) %>%
        mutate(n=tf_idf) %>%
        select(-tf_idf)
    }
    
    crs_join <- merge(unique(data %>% select(ID)), feature$bigram, by=NULL) %>%
      mutate(bigram=y) %>%
      select(-y) %>%
      arrange(as.numeric(ID))
    
    ft_vec <- as.character(t(feature$bigram))
    
    data <- data %>%
      filter(bigram %in% ft_vec)
    
    if(pur=="train"){
      data <- data %>%
        select(-Class) }
    
    lft_join <- merge(crs_join, data, all.x="TRUE") %>%
      arrange(as.numeric(ID))
    lft_join[is.na(lft_join)] <- 0
    
    tab <- dcast(lft_join, ID~bigram, value.var="n", fill=0) %>%
      arrange(as.numeric(ID))
  }
  
  return(tab)
}
# Creating probability table for each feature with given frequency table
prob_mat <- function(freq_tab=x){
  den <- freq_tab %>%
    select(-ID) %>%
    sum()
  
  num <- freq_tab %>%
    select(-ID) %>%
    apply(2, sum)
  aa <- (num+1)/(den+length(num))
  return(matrix(aa))
}
softmax <- function(x){
  return(exp(x+max(x))/sum(exp(x+max(x))))
}

## Training the naive bayes model

naive_baysean <- function(feature=class_word, data=tr_word_token, by="n", token="word"){
  
  model <- data.frame(matrix(nrow=20, ncol=0))
  for(i in c(1:9)){
    train_table <- freq_table(feature=feature, data=data, by=by, token=token, i=i, pur="train")
    temp <- prob_mat(train_table)
    model <- data.frame(cbind(model, temp))
  }
  names(model) <- as.character(c(1:9))
  
  return(model)  
}
nb_predict <- function(nb_model, feature=class_word, data=te_word_token, by="n", token="word"){
  result <- data.frame(matrix(nrow=length(unique(data$ID)),ncol=0))
  test_table <- freq_table(feature=feature, data=data, by=by, token=token, i=1, pur="test")
  test_id <- test_table[,1]
  
  for(i in c(1:9)){
    test_table <- freq_table(feature=feature, data=data, by=by, token=token, i=i, pur="test")
    test_data <- test_table[,-1]

    temp <- as.matrix(test_data) %*% as.matrix(nb_model[,i])
    result <- data.frame(cbind(result, temp))
  }
  
  result <- t(apply(result, 1, softmax))
  result <- data.frame(cbind(as.numeric(test_id), result))
  names(result) <- c("ID", as.character(c(1:9)))
  
  return(result)
}



##multi-class svm

svm_table <- function(feature=x, data=y, by=z, token=w, i=i, pur=pur){
  
  if(pur=="train"){
    cl_info <- data %>%
      select(ID, Class) %>%
      distinct()
  }
  
  feature <- feature %>% 
    filter(Class==i)
  
  if(token=="word"){
    if(by=="tf_idf"){
      feature <- feature %>%
        mutate(n=tf_idf) %>%
        select(-tf_idf)
      
      data <- data %>%
        bind_tf_idf(word, ID, n) %>%
        select(-n, -tf, -idf) %>%
        mutate(n=tf_idf) %>%
        select(-tf_idf)
      data[is.na(data$n),] <- 0
    }
    
    crs_join <- merge(unique(data %>% select(ID)), feature$word, by=NULL) %>%
      mutate(word=y) %>%
      select(-y) %>%
      arrange(as.numeric(ID))
    
    ft_vec <- as.character(unique(feature$word))
    
    data <- data %>%
      filter(word %in% ft_vec)
    
    lft_join <- merge(crs_join, data, all.x="TRUE") %>%
      arrange(as.numeric(ID))
    lft_join[is.na(lft_join)] <- 0
    
    tab <- dcast(lft_join, ID~word, value.var="n", fill=0) %>%
      arrange(as.numeric(ID))
  } 
  
  if(token=="bigram"){
    if(by=="tf_idf"){
      feature <- feature %>%
        mutate(n=tf_idf) %>%
        select(-tf_idf)
      
      data <- data %>%
        bind_tf_idf(bigram, ID, n) %>%
        select(-n, -tf, -idf) %>%
        mutate(n=tf_idf) %>%
        select(-tf_idf)
      data[is.na(data$n),] <- 0
    }
    
    crs_join <- merge(unique(data %>% select(ID)), feature$bigram, by=NULL) %>%
      mutate(bigram=y) %>%
      select(-y) %>%
      arrange(as.numeric(ID))
    
    ft_vec <- as.character(t(feature$bigram))
    
    data <- data %>%
      filter(bigram %in% ft_vec)
    
    lft_join <- merge(crs_join, data, all.x="TRUE") %>%
      arrange(as.numeric(ID))
    lft_join[is.na(lft_join)] <- 0
    
    tab <- dcast(lft_join, ID~bigram, value.var="n", fill=0) %>%
      arrange(as.numeric(ID))
  }
  
  if(pur=="train"){
    tab <- tab %>%
      merge(cl_info, by="ID")
    tab[which(tab$Class!=i),]$Class <- 0
    tab <- tab %>% 
      mutate(Class=as.factor(Class))
  }
  
  tab <- tab %>%
    mutate(ID=as.numeric(ID)) %>%
    arrange(ID)
  
  if(pur=="train"){
    v <- c()
    for(j in c(2:ncol(tab)-1)){
      if(sum(as.numeric(tab[,j]))==0){ v <- c(v,j)}
    }
    if(length(v)!=0){ tab <- tab[,-v] }
  }
  
  return(tab)
}
multi_class_svm <- function(feature=feature, tr_data=tr_data, te_data=te_data, by=by, token=token, kern=kern){
  
  result <- data.frame(matrix(nrow=length(unique(te_data$ID)),ncol=0))
  test_id <- sort(as.numeric(unique(te_data$ID)))
  for(i in c(1:9)){
    svm_train <- svm_table(feature=feature, data=tr_data, by=by, token=token, i=i, pur="train")
    svm_test <- svm_table(feature=feature, data=te_data, by=by, token=token, i=i, pur="test")
    svm_test <- svm_test[,(names(svm_test) %in% names(svm_train))]
    
    svm_model <- ksvm(Class~., data=svm_train[,-1], kernel=kern, prob.model=TRUE)
    svm_temp <- predict(svm_model, svm_test[,-1], type="probabilities")
    
    result <- data.frame(cbind(result, svm_temp[,2]))
  }
  result <- t(apply(result, 1, softmax))
  result <- data.frame(cbind(test_id, result))
  names(result) <- c("ID", as.character(c(1:9)))
  
  return(result)
}

#checking the result 

max_class <- function(x){
  temp_id <- x[,1]
  temp <- apply(x[,-1], 1, function(y){ return(names(y)[which(y==max(y))][1]) })
  temp <- data.frame(cbind(temp_id, unlist(temp)))
  names(temp) <- c("ID","Class")
  
  return(temp)
}
result_table <- function(pred_result, te_label){
  res <- max_class(pred_result)
  res$Class <- factor(res$Class, levels = c(1:9))
  print(table(res$Class, te_label$Class, dnn=c("predicted","actual")))
  print(table(res$Class==te_label$Class))
  
  result_table <- data.frame(as.matrix(table(res$Class, te_label$Class, dnn=c("predicted","actual")), ncol=9))
  res$Class
  precision_recall <- result_table %>%
    group_by(predicted) %>%
    mutate(pre_sum=sum(Freq)) %>%
    ungroup() %>%
    group_by(actual) %>%
    mutate(act_sum=sum(Freq)) %>%
    ungroup() %>%
    filter(predicted==actual) %>%
    mutate(precision=Freq/pre_sum) %>%
    mutate(recall=Freq/act_sum) %>%
    mutate(Class=actual) %>%
    select(Class, precision, recall)
  

  return(precision_recall)
}

# Creating the training set using taining and validation set 

set.seed(171213)
sam_num <- sample(nrow(trv), 2200)
ID_list <- sort(unique(trv$ID))
tr_num <- ID_list[sam_num]
te_num <- ID_list[-sam_num]

tr_word_data <- tr_word_token %>%
  filter(ID %in% tr_num)

te_word_data <- tr_word_token %>%
  filter(ID %in% te_num) %>%
  select(-Class)

te_word_label <- tr_word_token %>%
  filter(ID %in% te_num) %>%
  select(ID, Class) %>%
  mutate(ID=as.numeric(ID)) %>%
  arrange(ID) %>%
  distinct()

tr_bigram_data <- tr_bigram_token %>%
  filter(ID %in% tr_num)

te_bigram_data <- tr_bigram_token %>%
  filter(ID %in% te_num) %>%
  select(-Class)

te_bigram_label <- tr_bigram_token %>%
  filter(ID %in% te_num) %>%
  select(ID, Class) %>%
  mutate(ID=as.numeric(ID)) %>%
  arrange(ID) %>%
  distinct()

result_compare <- data.frame(cbind(c(1:9),matrix(nrow=9,ncol=0)))

# naive_bayesian model

# model using word & n
nb_model <- naive_baysean(feature=class_word, data=tr_word_data, by="n", token="word")
nb_result <- nb_predict(nb_model, feature=class_word, data=te_word_data, by="n", token="word")
temp <- result_table(nb_result, te_word_label) #accuracy : 0.3925
result_compare <- cbind(result_compare, temp[,-1])

#model using word & tf_idf
nb_model <- naive_baysean(feature=class_word_tf, data=tr_word_data, by="tf_idf", token="word")
nb_result <- nb_predict(nb_model, feature=class_word_tf, data=te_word_data, by="tf_idf", token="word")
temp <- result_table(nb_result, te_word_label) #accuracy : 0.1373
result_compare <- cbind(result_compare, temp[,-1])

#model using bigram & n
nb_model <- naive_baysean(feature=class_bigram, data=tr_bigram_data, by="n", token="bigram")
nb_result <- nb_predict(nb_model, feature=class_bigram, data=te_bigram_data, by="n", token="bigram")
temp <- result_table(nb_result, te_bigram_label) #accuracy : 0.3951
result_compare <- cbind(result_compare, temp[,-1])

#model using bigram & tf_idf
nb_model <- naive_baysean(feature=class_bigram_tf, data=tr_bigram_data, by="tf_idf", token="bigram")
nb_result <- nb_predict(nb_model, feature=class_bigram_tf, data=te_bigram_data, by="tf_idf", token="bigram")
temp <- result_table(nb_result, te_bigram_label) #accuracy : 0.3925
result_compare <- cbind(result_compare, temp[,-1])


# Support_vector_machine model

#model using word & n
svm_result <- multi_class_svm(feature=class_word, tr_data=tr_word_data, te_data=te_word_data, by="n", token="word", kern="rbfdot")
temp <- result_table(svm_result, te_word_label) #accuracy : 0.5646
result_compare <- cbind(result_compare, temp[,-1])

#model using word & tf_idf
svm_result <- multi_class_svm(feature=class_word_tf, tr_data=tr_word_data, te_data=te_word_data, by="tf_idf", token="word", kern="rbfdot")
temp <- result_table(svm_result, te_word_label) #accuracy : 0.3006
result_compare <- cbind(result_compare, temp[,-1])

#model using bigram & n
svm_result <- multi_class_svm(feature=class_bigram, tr_data=tr_bigram_data, te_data=te_bigram_data, by="n", token="bigram", kern="rbfdot")
temp <- result_table(svm_result, te_bigram_label) #accuracy : 0.5414
result_compare <- cbind(result_compare, temp[,-1])

#model using bigram & tf_idf
svm_result <- multi_class_svm(feature=class_bigram_tf, tr_data=tr_bigram_data, te_data=te_bigram_data, by="tf_idf", token="bigram", kern="rbfdot")
temp <- result_table(svm_result, te_bigram_label) #accuracy : 0.4549
result_compare <- cbind(result_compare, temp[,-1])


head(te_dcg)
aa <- tr_dcg[,-1]
aa$Class <- as.factor(aa$Class)
svm_model <- ksvm(Class~., data=aa, kernel="rbfdot", prob.model=TRUE)
svm_temp <- predict(svm_model, te_dcg[,-1], type="probabilities")
head(max_class(svm_temp))
?max.col

table(max.col(svm_temp), te_word_label$Class)
table(max.col(svm_temp)==te_word_label$Class)
head(te_word_label)

result_table(svm_result, te_word_label)

















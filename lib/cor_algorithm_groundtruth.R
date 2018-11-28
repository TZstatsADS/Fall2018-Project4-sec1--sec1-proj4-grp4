### author Jiansong Chen  
### Project4 Group 4


alg_del <- function(word){
  n <- nchar(word) 
  out <- list()
  temp <- list()
  wordstr <- unlist(strsplit(word, NULL))
  index <- 0
  for (i in 1:length(wordstr)){
    if (wordstr[i] %in% letters) {
      index <- index + 1
    }
  }
  if (index != n) {
    return(c(word,0))
  }
  for (letter in letters) {
    out[[letter]] <- rep(word, n + 1)
    temp[[letter]] <- rep(NA, n + 1)
    for (i in 1:(n + 1)) {
      out[[letter]][i] <- paste(substr(word, i - n, i - 1), letter, substr(word, i, n), sep = "")
      if ( is.null(corplist_word_groundtruth[[out[[letter]][i]]]) == T){
        corpus_word <- 0
      } else {
        corpus_word <- corplist_word_groundtruth[[out[[letter]][i]]]
      }
      if (i == 1){
        temp[[letter]][1] <- del_matrix["@",letter]/corplist_char_groundtruth[[letter]]*(corpus_word+0.5)/(N_groundtruth+V_groundtruth/2)
      } else {
        temp[[letter]][i] <- del_matrix[wordstr[i-1],letter]/corplist_char_groundtruth[[paste(wordstr[i-1],letter,sep = "")]]*(corpus_word+0.5)/(N_groundtruth+V_groundtruth/2)
      }
    }
  }
  out <- unlist(out)
  temp <- unlist(temp)
  for (i in 1:length(temp)){
    if (is.nan(temp[i]) == T){
      temp[i] <- 0
    }
    if(temp[i] == Inf){
      temp[i] <- 0
    }
  }
  for (i in 1:length(temp)){
    if (temp[i] == max(temp)){
      return(c(out[i],max(temp)))
    }
  }
}


alg_ins <- function(word){
  out <- 0
  index <- 0
  temp <- 0
  n <- nchar(word) 
  wordstr <- unlist(strsplit(word, NULL))
  index <- 0
  for (i in 1:length(wordstr)){
    if (wordstr[i] %in% letters) {
      index <- index + 1
    }
  }
  if (index != n) {
    return(c(word,0))
  }
  if (n == 1){
    return(c("", 0))
  }else{
    for(i in 1:n) {
      out[i] <- paste(wordstr[-i], collapse = "")
      if ( is.null(corplist_word_groundtruth[[out[i]]]) == T){
        corpus_word <- 0
      } else {
        corpus_word <- corplist_word_groundtruth[[out[i]]]
      }
      if (i == 1){
        temp[1] <- insert_matrix["@",wordstr[1]]/corplist_char_groundtruth[[wordstr[1]]]*(corpus_word+0.5)/(N_groundtruth+V_groundtruth/2)
      } else {
        temp[i] <- insert_matrix[wordstr[i-1],wordstr[i]]/corplist_char_groundtruth[[wordstr[i]]]*(corpus_word+0.5)/(N_groundtruth+V_groundtruth/2)
      }
    }
    for (i in 1:length(temp)){
      if (is.nan(temp[i]) == T){
        temp[i] <- 0
      }
      if(temp[i] == Inf){
        temp[i] <- 0
      }
    }
    for (i in 1:n){
      if (temp[i] == max(temp)){
        return(c(out[i],max(temp)))
      }
    }
  }
}


alg_subs <- function(word){
  n <- nchar(word) 
  out <- list()
  temp <- list()
  wordstr <- unlist(strsplit(word, NULL))
  index <- 0
  for (i in 1:length(wordstr)){
    if (wordstr[i] %in% letters) {
      index <- index + 1
    }
  }
  if (index != n) {
    return(c(word,0))
  }
  for (letter in letters) {
    out[[letter]] <- rep(word, n)
    for (i in 1:n) {
      out[[letter]][i] <- paste(substr(word, i - n, i - 1), letter, 
                                substr(word, i + 1, n + 1), sep = "")
      if ( is.null(corplist_word_groundtruth[[out[[letter]][i]]]) == T){
        corpus_word <- 0
      } else {
        corpus_word <- corplist_word_groundtruth[[out[[letter]][i]]]
      }
      temp[[letter]][i] <- sub_mat[wordstr[i],letter]/corplist_char_groundtruth[[letter]]*(corpus_word+0.5)/(N_groundtruth+V_groundtruth/2)
    }
  }
  out <- unlist(out)
  temp <- unlist(temp)
  for (i in 1:length(temp)){
    if (is.nan(temp[i]) == T){
      temp[i] <- 0
    }
    if(temp[i] == Inf){
      temp[i] <- 0
    }
  }
  for (i in 1:length(temp)){
    if (temp[i] == max(temp)){
      return(c(out[i],max(temp)))
    }
  }
}

alg_rev <- function(word){
  n <- nchar(word)
  temp <- 0
  wordstr <- unlist(strsplit(word, NULL))
  index <- 0
  for (i in 1:length(wordstr)){
    if (wordstr[i] %in% letters) {
      index <- index + 1
    }
  }
  if (index != n) {
    return(c(word,0))
  }
  if (n > 2) {
    out <- rep(wordstr, n - 1)
    perms <- matrix(c(1:(n - 1), 2:n), ncol = 2)
    reversed <- perms[, 2:1]
    trans.words <- matrix(rep(wordstr, n - 1), byrow = TRUE, nrow = n - 1)
    for(i in 1:(n - 1)) {
      trans.words[i, perms[i, ]] <- trans.words[i, reversed[i, ]]
      out[i] <- paste(trans.words[i, ], collapse = "")
      if ( is.null(corplist_word_groundtruth[[out[i]]]) == T){
        corpus_word <- 0
      } else {
        corpus_word <- corplist_word_groundtruth[[out[i]]]
      }
      temp[i] <- rev_mat[wordstr[i+1],wordstr[i]]/corplist_char_groundtruth[[paste(wordstr[i+1],wordstr[i],sep="")]]*(corpus_word+0.5)/(N_groundtruth+V_groundtruth/2)
    }
    for (i in 1:length(temp)){
      if (is.nan(temp[i]) == T){
        temp[i] <- 0
      }
      if(temp[i] == Inf){
        temp[i] <- 0
      }
    }
    for (i in 1:n){
      if (temp[i] == max(temp)){
        return(c(out[i],max(temp)))
      }
    }
  }else if (n == 2) {
    out <- paste(wordstr[2:1], collapse = "")
    if ( is.null(corplist_word_groundtruth[[out]]) == T){
      corpus_word <- 0
    } else {
      corpus_word <- corplist_word_groundtruth[[out]]
    }
    temp <- rev_mat[wordstr[2],wordstr[1]]/corplist_char_groundtruth[[out]]*(corpus_word+0.5)/(N_groundtruth+V_groundtruth/2)
    return(c(out,temp))
  }else {
    out <- paste(wordstr, collapse = "")
    return(c(out,0))
  }
}

setwd("C:/Users/cjsly/Documents/GitHub/Fall2018-Project4-sec1--sec1-proj4-grp4")
load("./lib/Groundtruth_corpus.RData")
load("./lib/ConfusionMatrice_groundtruth.RData")
library(hash)

## load dectected error words
cor_word <- function(word){
  for (i in 1:length(word)) {
    result <- rbind(alg_del(word[i]),alg_ins(word[i]),alg_rev(word[i]),alg_subs(word[i]))
    words <- result[,1]
    scores <- as.numeric(result[,2])
    for (i in 1:4){
      if (scores[i] == max(scores)){
        return(words[i])
      }
    }
  }
}

cor_word("adminstrador")
word <- "applt"
alg_del(word)
alg_ins(word)
alg_rev(word)
alg_subs(word)

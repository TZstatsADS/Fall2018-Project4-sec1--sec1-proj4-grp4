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
      if ( is.null(corplist_word[[out[[letter]][i]]]) == T){
        corpus_word <- 0
      } else {
        corpus_word <- corplist_word[[out[[letter]][i]]]
      }
      if (i == 1){
        temp[[letter]][1] <- confusion_del["#",letter]/corplist_char[[letter]]*(corpus_word+0.5)/(N+V/2)
      } else {
        temp[[letter]][i] <- confusion_del[wordstr[i-1],letter]/corplist_char[[paste(wordstr[i-1],letter,sep = "")]]*(corpus_word+0.5)/(N+V/2)
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
      if ( is.null(corplist_word[[out[i]]]) == T){
        corpus_word <- 0
      } else {
        corpus_word <- corplist_word[[out[i]]]
      }
      if (i == 1){
        temp[1] <- confusion_ins["#",wordstr[1]]/corplist_char[[wordstr[1]]]*(corpus_word+0.5)/(N+V/2)
      } else {
        temp[i] <- confusion_ins[wordstr[i-1],wordstr[i]]/corplist_char[[wordstr[i]]]*(corpus_word+0.5)/(N+V/2)
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
      if ( is.null(corplist_word[[out[[letter]][i]]]) == T){
        corpus_word <- 0
      } else {
        corpus_word <- corplist_word[[out[[letter]][i]]]
      }
      temp[[letter]][i] <- confusion_sub[wordstr[i],letter]/corplist_char[[letter]]*(corpus_word+0.5)/(N+V/2)
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
      if ( is.null(corplist_word[[out[i]]]) == T){
        corpus_word <- 0
      } else {
        corpus_word <- corplist_word[[out[i]]]
      }
      temp[i] <- confusion_rev[wordstr[i+1],wordstr[i]]/corplist_char[[paste(wordstr[i+1],wordstr[i],sep="")]]*(corpus_word+0.5)/(N+V/2)
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
    if ( is.null(corplist_word[[out]]) == T){
      corpus_word <- 0
    } else {
      corpus_word <- corplist_word[[out]]
    }
    temp <- confusion_rev[wordstr[2],wordstr[1]]/corplist_char[[out]]*(corpus_word+0.5)/(N+V/2)
    return(c(out,temp))
  }else {
    out <- paste(wordstr, collapse = "")
    return(c(out,0))
  }
}
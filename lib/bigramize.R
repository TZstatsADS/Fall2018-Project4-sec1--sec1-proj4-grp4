##############################################################
##############################################################
##############################################################

### Authors: Binglun Zhao bz2342
### Project 4 Group 4


bigramize_word <- function(word) {
  results <- list()
  n <- nchar(word)
  k <- 0
  list_names <- c()
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      k <- k + 1
      results[[k]] <- paste(substring(word,i,i),substring(word,j,j),sep='')
      list_names[k] <- paste('PD_',i,'_',j,sep='')
    }
  }
  names(results) <- list_names
  return(results)
}

bigramize_words <- function(words) {
  results <- sapply(words,bigramize_word)
  results <- apply(results,1,unlist)
  return(t(results))
}

bigramize <- function(words) {
  l <- nchar(words[1])
  if(length(words)==1) {
    results <- bigramize_word(words)
  } else {
    tmp_results <- bigramize_words(words)
    n <- dim(tmp_results)[1]
    results <- list()
    for (i in 1:n) {
      results[[i]] <- tmp_results[i,]
    }
    list_names <- c()
    for (i in 1:(l-1)) {
      for (j in (i+1):l) {
        list_names <- c(list_names, paste('PD_',i,'_',j,sep=''))
      }
    }
    names(results) <- list_names
  }
  return(results)
}







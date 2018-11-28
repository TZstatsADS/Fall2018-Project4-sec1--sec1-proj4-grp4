##############################################################
##############################################################
##############################################################

### Authors: Binglun Zhao bz2342
### Project 4 Group 4


### function for labels
make_label <- function(word,dictionary) {
  return(as.numeric(word%in%dictionary))
}

### functions for feature extraction
cal_naturalness <- function(word,bigram_dic,c=10000) {
  results <- unlist(bigramize_word(word))
  n <- length(results)
  freq <- c()
  for (i in 1:n) {
    if (results[i]%in%bigram_dic$gt_bigram) {
      freq[i] <- bigram_dic$Freq[bigram_dic$gt_bigram==results[i]]
    } else {freq[i] <- 0}
  }
  bigr <- (sum(freq)/c)/n
  return(bigr)
}

max_repeating <- function(word) {
  l <- nchar(word)
  cur_count <- c()
  for (i in 1:l) {
    #cat("i=",i,"\n",sep='')
    cur_letter <- substr(word,i,i)
    cur_count[i] <- 1
    j <- i + 1
    while (substr(word,j,j)==cur_letter) {
      #cat("j=",j,"\n",sep='')
      cur_count[i] <- cur_count[i] + 1
      j <- j + 1
    }
  }
  count <- max(cur_count)
  return(count)
}

extract_feature <- function(word,bigram_dic,c=10000) {
  l <- nchar(word)
  v <- str_count(word,'[aeiouAEIOU]')
  c <- str_count(word,'[bcdfghjklmnpqrstvxzwyBCDFGHJKLMNPQRSTVXZWY]')
  s <- str_count(word,"[^[:alnum:]]")
  d <- str_count(word,"[0-9]")
  low <- str_count(word,"[:lower:]")
  upp <- str_count(word,"[:upper:]")
  consec <- max_repeating(word)
  ## alnum <- str_count(word,"[:alnum:]")
  feat7 <- as.numeric(str_count(word,"[:alnum:]")<s)
  feat8 <- as.numeric(str_count(word,"[bcdfghjklmnpqrstvxzwyBCDFGHJKLMNPQRSTVXZWY]{6,}")>=6)
  tmp_word <- substr(word,2,(l-1))
  feat9 <- as.numeric(str_count(tmp_word,"[^[:alnum:]]")>=2)
  naturalness <- cal_naturalness(word,bigram_dic,c)
  most_freq <- ifelse(max(table(strsplit(word,"")))>=3,max(table(strsplit(word,"")))/l,0)
  non_alpha <- (l-str_count(word,"[:alpha:]"))/str_count(word,"[:alpha:]")
  feat <- c("v/l"=v/l,
            "c/l"=c/l,
            "v/c"=v/c,
            "s/l"=s/l,
            "d/l"=d/l,
            "low/l"=low/l,
            "upp/l"=upp/l,
            consec=consec,
            feat7=feat7,
            feat8=feat8,
            feat9=feat9,
            naturalness=naturalness,
            most_freq=most_freq,
            non_alpha=non_alpha)
  return(feat)
}


library(tm)
library(topicmodels)
library(dplyr)
library(tidytext)

data("AssociatedPress",package = "topicmodels")

terms <- Terms(AssociatedPress) #extract the unqiue terms 

dict <- data.frame(terms) # the dictionary 
colnames(dict) <- 'words' # create matrix for dictionary, and colname is 'words'

ap_td <- tidy(AssociatedPress)
ap_td  #the matrix: the document, the term, the count 
corp <- ap_td %>% 
  count(term, sort =F, wt=count) 
head(corp, 20) # the frequency of the term 


################## Calculate the frequency for characters and bigrams ################
strcount <- function(x, pattern){
  unlist(lapply(strsplit(x, NULL),function(z) na.omit(length(grep(pattern, z)))))
}

bi_mat<- matrix(0,26,26)
colnames(bi_mat)<- letters
rownames(bi_mat)<- letters

search_for_bi<- function(bi){
  total<- 0
  for(i in corp$term){
    num<- sum(gregexpr(bi, i, fixed=TRUE)[[1]] > 0)
    total<- total+num
  }
  return(total)
}

corp_char<- matrix(NA,nrow=1,ncol=702)
cc<- c()
for(i in letters){
  for(j in letters){
    cc<- c(cc,paste0(i,j))
  }
}
colnames(corp_char)<- c(letters,cc)
for(i in letters){
  for(j in letters){
    bi<- paste0(i,j)
    num<- search_for_bi(bi)
    corp_char[1,bi]<- num
  }
}

for (i in letters) {
  total<- 0
  for(j in corp$term){
    num<- sum(gregexpr(i, j, fixed=TRUE)[[1]] > 0)
    total<- total+num
  }
  corp_char[1,i]<- total
}

library('hash')
corplist_word<- hash(keys=corp$term,values=corp$n)
corplist_char<- hash(keys=colnames(corp_char),values=corp_char[1,])
N<- sum(corp$n)
V<-length(corp$term)


save(corp,N,V,corplist_word,corplist_char,file = 'APcorpus.RData')
# corp is the corpus of all articles
# N is the total number of words
# V is the number of unique words
# corplist_word is a list of word and its corresponding frequencies
# corplist_char is a list of the single characater, bigrams and their corresponding frequencies




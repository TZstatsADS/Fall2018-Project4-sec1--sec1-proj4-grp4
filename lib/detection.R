
library(e1071)
library(stringr)
library(reshape2) # melt

setwd("/Users/zhaobinglun/Documents/GitHub/Fall2018-Project4-sec1--sec1-proj4-grp4/doc")

source("../lib/zbl/functions.R")
source("../lib/zbl/get_text.R")
source("../lib/zbl/bigramize.R")

gt_dir <-"../data/ground_truth"
ta_dir <-"../data/tesseract"

gt_file_name <- list.files(groud_truth_dir)
gt_file_path <- paste0(groud_truth_dir,'/',gt_file_name)
gt_words <- lapply(gt_file_path,get_text)
gt_dictionary <- unlist(gt_words)

ta_file_name <- list.files(tesseract_dir)
ta_file_path <- paste0(tesseract_dir,'/',ta_file_name)
ta_words <- lapply(ta_file_path,get_text)
ta_dictionary <- unlist(ta_words)

labels_tm <- NA
labels_tm <- system.time(ta_labels <- lapply(ta_dictionary,make_label,gt_dictionary))
save(ta_labels,file="../lib/zbl/lable.Rdata")
load("../lib/zbl/lable.Rdata")

# word <- "b!!#@bba{aoionO...000I#dkkjs^^^10UNd..ddd~SI38@!!go./,$.;[{(]})"
# str_count(word,'[a-zA-Z]')
# word <- "~!@#$%^&*()_+=-`{}|][;'/.,<>\"?:\\"
# word <- '!ab(1!3#D~:.(8OdR'
l <- nchar(word)
v <- str_count(word,'[aeiouAEIOU]')
c <- str_count(word,'[bcdfghjklmnpqrstvxzwyBCDFGHJKLMNPQRSTVXZWY]')
s <- str_count(word,"[^[:alnum:]]")
d <- str_count(word,"[0-9]")
low <- str_count(word,"[:lower:]")
upp <- str_count(word,"[:upper:]")
#consec <- str_count(word,".{3,}")
word <- "aaaaaaaaaaaabddc"
alnum <- str_count(word,"[:alnum:]")
feat7 <- as.numeric(str_count(word,"[:alnum:]")<s)
feat8 <- as.numeric(str_count(word,"[bcdfghjklmnpqrstvxzwyBCDFGHJKLMNPQRSTVXZWY]{6,}")>=6)
tmp_word <- substr(word,2,(l-1))
feat9 <- as.numeric(str_count(tmp_word,"[^[:alnum:]]")>=2)

gt_by_length <- sort_by_length(tolower(gt_dictionary))
num_len <- length(gt_by_length)
gt_bigram_from_3 <- lapply(gt_by_length[3:num_len],bigramize)
gt_bigram_from_3 <- melt(gt_bigram_from_3)
save(gt_bigram_from_3,file="../lib/zbl/gt_bigram_from_3.Rdata")
load("../lib/zbl/gt_bigram_from_3.Rdata")
gt_bigram <- c(gt_bigram_from_3[,1],gt_by_length[[2]])
gt_table <- table(gt_bigram)
gt_table <- as.data.frame(gt_table)
save(gt_table,file="../lib/zbl/gt_table.Rdata")
load("../lib/zbl/gt_table.Rdata")
lapply(ta_dictionary,naturalness,gt_table,1)


words <- c("baidu",1997)



















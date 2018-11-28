
### load required library
library(stringr) #substr
library(reshape2) #melt
library(e1071) #SVM

### set working directory
### put this file to the doc folder
setwd("/Users/zhaobinglun/Documents/GitHub/Fall2018-Project4-sec1--sec1-proj4-grp4/doc")
# setwd("../Fall2018-Project4-sec1--sec1-proj4-grp4/doc")

### load the customized functions
### put these files to the lib folder
source("../lib/feat_label.R")
source("../lib/get_text.R")
source("../lib/bigramize.R")

### contruct the ground truth dictionary
gt_dir <-"../data/ground_truth"
gt_file_name <- list.files(gt_dir)
gt_file_path <- paste0(gt_dir,'/',gt_file_name)
gt_words <- lapply(gt_file_path,get_text)
gt_dictionary <- unlist(gt_words)

### contruct the tesseract dictionary
ta_dir <-"../data/tesseract"
ta_file_name <- list.files(ta_dir)
ta_file_path <- paste0(ta_dir,'/',ta_file_name)
ta_words <- lapply(ta_file_path,get_text)
ta_dictionary <- unlist(ta_words)

### generate labels
### either by these comment-out codes or laoding from the Rdata file
labels_tm <- NA
labels_tm <- system.time(ta_labels <- unlist(lapply(ta_dictionary,make_label,gt_dictionary)))
lab_mat <- cbind(ta_dictionary,ta_labels)
# names(ta_labels) <- ta_dictionary
save(ta_labels,file="../output/ta_labels.Rdata")
load("../output/ta_lable.Rdata")
save(lab_mat,file="../output/lab_mat.Rdata")
load("../output/lab_mat.Rdata")

#######################################
# word <- "baidklajskdjfk;hj1!*Y*@&#Nmndl,JKJKJD"
# l <- nchar(word)
# v <- str_count(word,'[aeiouAEIOU]')
# c <- str_count(word,'[bcdfghjklmnpqrstvxzwyBCDFGHJKLMNPQRSTVXZWY]')
# s <- str_count(word,"[^[:alnum:]]")
# d <- str_count(word,"[0-9]")
# low <- str_count(word,"[:lower:]")
# upp <- str_count(word,"[:upper:]")
# consec <- max_repeating(word)
# alnum <- str_count(word,"[:alnum:]")
# feat7 <- as.numeric(str_count(word,"[:alnum:]")<s)
# feat8 <- as.numeric(str_count(word,"[bcdfghjklmnpqrstvxzwyBCDFGHJKLMNPQRSTVXZWY]{6,}")>=6)
# tmp_word <- substr(word,2,(l-1))
# feat9 <- as.numeric(str_count(tmp_word,"[^[:alnum:]]")>=2)
# naturalness <- cal_naturalness(word,gt_table,1)
# most_freq <- ifelse(max(table(strsplit(word,"")))>=3,max(table(strsplit(word,"")))/l,0)
# non_alpha <- str_count(word,"[:alpha:]")/(l-str_count(word,"[:alpha:]"))

#######################################

### note that only convert the dictionary to lowercase but not remove punctuations or other symbols

### sort the ground trurh dictionary by letter length
gt_by_length <- sort_by_length(tolower(gt_dictionary))

### bigramize ground trurh dictionary
### either by these comment-out codes or laoding from the Rdata file
# num_len <- length(gt_by_length)
# gt_bigram_from_3 <- lapply(gt_by_length[3:num_len],bigramize)
# gt_bigram_from_3 <- melt(gt_bigram_from_3)
# save(gt_bigram_from_3,file="../output/gt_bigram_from_3.Rdata")
load("../output/gt_bigram_from_3.Rdata")

### convert bigramized ground trurh dictionary to tabel
### either by these comment-out codes or laoding from the Rdata file
# gt_bigram <- c(gt_bigram_from_3[,1],gt_by_length[[2]])
# gt_table <- table(gt_bigram)
# gt_table <- as.data.frame(gt_table)
# save(gt_table,file="../output/gt_table.Rdata")
load("../output/gt_table.Rdata")

### construct the feature lists of tesseract dictionary
### either by these comment-out codes or laoding from the Rdata file
feature_tm <- NA
feature_tm <- system.time(feat_list <- lapply(ta_dictionary,extract_feature,gt_table,1))
save(feature_tm,file="../output/feat_list.Rdata")
load("../output/feat_list.Rdata")

### convert the feature lists to feature matrix
### either by these comment-out codes or laoding from the Rdata file
feat_mat <- do.call(rbind,lapply(feat_list,matrix,ncol=14,byrow=TRUE)) 
colnames(feat_mat)<- names(feat_list[[1]])
save(feature_tm,file="../output/feat_mat.Rdata")
load("../output/feat_mat.Rdata")

colnames(feat_mat[,10])






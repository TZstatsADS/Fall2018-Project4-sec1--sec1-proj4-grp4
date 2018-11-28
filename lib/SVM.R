#######################################
### Project 4 Group 4
### SVM Evaluation
### Author: Sen Fu
#######################################
### Data Prepare for SVM 
feat_mat<-as.data.frame(feat_mat)
# Replace NaN and Inf with 0 and 99999 respectively
feat_mat[feat_mat=="NaN"]<-0
feat_mat[feat_mat=="Inf"]<-99999
ta_labels_df <- data.frame(matrix(unlist(ta_labels), nrow = 298728, byrow = TRUE))
colnames(ta_labels_df)<-"garbage"
#######################################
### Model Setup
### For all the data, we use 80% as trainset
### Ideally we would use 20% remaining as testset. But for accuracy as well as serving as a dictionary
### purpose, we use 100% as testset
feat_mat_all <- feat_mat[1:298728,]
ta_labels_df_all <- ta_labels_df[1:298728,]
feat_mat_all <- cbind(feat_mat_all,ta_labels_df_all)
colnames(feat_mat_all)[15]<-c("garbage")
feat_mat_all$garbage[feat_mat_all$garbage==1]<-2
feat_mat_all$garbage[feat_mat_all$garbage==0]<-1
feat_mat_all$garbage[feat_mat_all$garbage==2]<-0
# Split data into a train and test set
index <- 1:nrow(feat_mat_all)
testindex <- sample(index, trunc(length(index)/5))
testset <- feat_mat_all[testindex,]
trainset <- feat_mat_all[-testindex,]
#######################################
### Model
### Fit the model and predict the testset values
### Because feat8 is constant, remove it from SVM
trainset$feat8 <- NULL
testset$feat8<-NULL
trainset$garbage<-as.factor(trainset$garbage)
feat_mat_all$feat8<-NULL
### Model without tuning 
# svm.model.1 <- svm(garbage ~., data = trainset, kernel = "linear", cost = 100, gamma = 1)
# svm.pred.1 <- predict(svm.model.1, testset[,-14])
# accuracy.1 <- mean(svm.pred.1==testset$garbage)
### Check Balance
balance <- sum(ta_labels_df$garbage)/nrow(ta_labels_df)
print(balance)
### Becasue balance is 66.6%, we consider the data is balance
# svm.pred.1 <- as.data.frame(svm.pred.1)
# True.Positive.1 <- 
# False.Positive.1 <-
# False.Negative.1 <-
# precision.1 <-
# recall.1 <-

#######################################
### Tuning with grid search
# model.tune <- tune(svm, garbage ~., data = trainset, range = list(cost = c(0.1,1,10,100), gamma = c(0.01,1,10,100,1000)))
#######################################
### Tuning with point test [find out that model 3 is the best one]
# svm.model.2 <- svm(garbage ~ ., data = trainset, cost = 10, gamma = 1)
# svm.pred.2 <- predict(svm.model.2, trainset[,-14])
# accuracy.2 <- mean(svm.pred.2==testset$garbage)

svm.model.3 <- svm(garbage ~ ., data = trainset, cost = 1, gamma = 1)
svm.pred.3 <- predict(svm.model.3, feat_mat_all[,-14])
accuracy.3 <- mean(svm.pred.3==testset$garbage)
save(svm.model.3, file = "svm.model.best.rda")
save(svm.pred.3, file = "svm.model.pred.rda")

# svm.model.4 <- svm(garbage ~ ., data = trainset, cost = 100, gamma = 0.1)
# svm.pred.4 <- predict(svm.model.4, testset[,-14])
# accuracy.4 <- mean(svm.pred.4==testset$garbage)
# 
# svm.model.5 <- svm(garbage ~ ., data = trainset, cost = 10, gamma = 0.1)
# svm.pred.5 <- predict(svm.model.5, testset[,-14])
# accuracy.5 <- mean(svm.pred.5==testset$garbage)
# 
# svm.model.6 <- svm(garbage ~ ., data = trainset, cost = 100, gamma = 0.01)
# svm.pred.6 <- predict(svm.model.6, testset[,-14])
# accuracy.6 <- mean(svm.pred.6==testset$garbage)
# 
# svm.model.7 <- svm(garbage ~ ., data = trainset, cost = 10, gamma = 0.01)
# svm.pred.7 <- predict(svm.model.7, testset[,-14])
# accuracy.7 <- mean(svm.pred.7==testset$garbage)
#######################################
# Prepare SVM output for Correction Section
svm.pred.3 <- as.data.frame(svm.pred.3)
ta_dictionary <- as.data.frame(ta_dictionary)
IndexID <- rownames(svm.pred.3)
rownames(svm.pred.3) <- NULL
svm.pred.3 <-cbind(IndexID, svm.pred.3)
IndexID <- rownames(ta_dictionary)
rownames(ta_dictionary) <- NULL
ta_dictionary <- cbind(IndexID, ta_dictionary)
svm.pred.3_dic <- merge.data.frame(svm.pred.3,ta_dictionary,by = "IndexID")
colnames(svm.pred.3_dic)[2]<- c("garbage")
svm.pred.3_dic<-subset(svm.pred.3_dic, select = -IndexID)
save(svm.pred.3_dic, file = "svm.pred.3_dic.rda")

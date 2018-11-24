
file_name_vec <- list.files("../data/ground_truth")
library(openxlsx)
missingline<- read.xlsx('AddLine_ver2.xlsx')
#missingline$FileName

truth_all<- c()
tess_all<- c()
for(i in c(1:length(file_name_vec))){
  truth<- readLines(paste0('./ground_truth/',file_name_vec[i]))
  tess<- readLines(paste0('./tesseract/',file_name_vec[i]))
  for (j in missingline$FileName) {
    if(file_name_vec[i]==j){
      addline<- as.numeric(missingline[which(missingline$FileName==j),"Line"])
      if(addline>length(tess)){
        tess<- c(tess,' ')
      }
      else{
        tess<- c(tess[1:(addline-1)],' ',tess[addline:length(tess)])
      }
    }
  }
  truth_all<- c(truth_all,truth)
  tess_all<- c(tess_all,tess)
  #print(file_name_vec[i])
  #print(length(truth)==length(tess))
  #write.table(tess,file = file_name_vec[i])
}

length(truth_all)
length(tess_all)

#error<- readLines('./tesseract/group1_00000005.txt')
#truth<- readLines('./ground_truth/group1_00000005.txt')
#txt<- writeLines(error,sep = "\n")
#num<- 0
#for(i in error){
#  splitted<- strsplit(i,' ')
#  splitted<- splitted[[1]]
#  num<- num+length(splitted)
#}
#print(num)




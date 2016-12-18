setwd('/home/imsrgadich/Documents/OneDrive/Aalto/Sem3&4/MSA/Ex 2')
mydata = read.table("DECATHLON.txt", header = TRUE)
mydata_mod = mydata[,-c(1,2,13,14)]  ## Remove the 1, 2, 13, 14 columns

mydata_mod_pca = princomp(mydata_mod,cor=FALSE)

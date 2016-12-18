covariance <- function(Input){
  
  sizeInput = dim(Input)[1]
  
  meanInput = colMeans(Input)
  
  modInput = sweep(Input,2,meanInput,"-")
  
  covInput = (as.matrix(t(modInput)) %*% as.matrix(modInput))/(sizeInput - 1)
  
  return(covInput)
  
}

setwd("/home/imsrgadich/Documents/OneDrive/Aalto/Sem3&4/MSA/Ex1")

mydata = read.table("Data1.txt")

cov_mydata = covariance(mydata)

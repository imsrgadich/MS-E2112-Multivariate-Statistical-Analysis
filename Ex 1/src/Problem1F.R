mydata = read.table("Data1.txt")

cent_plot(mydata)

cent_plot <- function(mydata){
  
  sizeData = dim(mydata)
  
  mean = colMeans(mydata)
  
  meanRep = kronecker(matrix(1,sizeData[1],1),matrix(mean,nrow=1))
  
  meanData = mydata - meanRep
  
  plot(meanData)
  
}




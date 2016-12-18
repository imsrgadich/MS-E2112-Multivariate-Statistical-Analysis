setwd("//home.org.aalto.fi/gadichs1/data/Desktop/P3_4/MSA/EX 9")

alcohol <- read.table("ALCOHOL.txt",header = T,sep = '\t')

#install.packages('MASS')

alcohol[,1] <- factor(alcohol[,1])

data = alcohol

data.lda <- lda(TYPE~.,data=data)

  # new data

newdata<- data.frame(MEOH=500,ACET = 400,BU1=3,MEPR=30,ACAL=20,LNPRO1=10)

predict(data.lda,newdata=newdata)$class

 


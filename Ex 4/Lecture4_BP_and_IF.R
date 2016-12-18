#robustness

library(MASS)

#generates a data set, mean vector (0, 10):
n <-20
A <-matrix(c(4,2,2,3), ncol=2)
d <- mvrnorm(n, c(0,10), A)

#lets take a look
d

###########################
#BPmean
############################

#just saves the data as a new variable:  
dc<- d

#lets take a look, dc is equal to d
dc

#lets make one of the points very nasty:
dc[n,1]<-10000000000
dc[n,2]<-10000000000

#lets take a look at our contaminated data

dc

#lets check Edistance
bdmean <- sqrt(sum((colMeans(d)- colMeans(dc))^2))


bdmean
#Based on this, is mean robust?

###########################
#empiricalIFmedian
############################
## change in 44
colMedian <- function(d){
  apply(d, MARGIN  = 2, median)
}

#Empirical influence function for median
IF<-function(x,d,n){ #eats a data set, a point, and n=sample size
d2<-rbind(d,x)#adds a point d to the data set
(n+1)*(colMedian(d2)-colMedian(d))#difference of the means
}

#point (0,0)
Y<-matrix(c(0,0), ncol=2)

#lets take a quick look
Y

for(i in 1:200){
x<- matrix(c(i,i), ncol=2)#new points (1,1), (2,2), (3,3) added
Y<-rbind(Y,IF(x,d,n))#influence function calculated (with points (i,i))
}


#Y now contains differences of the means


plot(Y[,1], type="l", col="blue")#first column
lines(Y[,2], col="red")#second column

#Based on this, is median robust?

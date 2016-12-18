setwd("//home.org.aalto.fi/gadichs1/data/Desktop/P3_4/MSA/Ex 10")

emp.data <- read.table('EMP.txt',header=T,sep = '\t',row.names = 1)

plot(emp.data)

## install packages fpc, MASS, cluster

library(fpc)
library(MASS)
library(cluster)

lab.1 <- c("TR","YU","GR","GR","BE","SE","CH","GB","WG","CS","HU")

plot(emp.data,panel = function(x,y) text(x,y,labels = lab,xpd=T))

# Turkey, Yuogslavia and sometimes Greece are a bit far from the others

emp.dist <- dist(emp.data,method="euclidean")

round(emp.dist,2)

min(emp.dist)

sort((round(emp.dist,1)))  #??

emp.min <- hclust(emp.dist,method = "single")

plot(emp.min)

emp.max <- hclust(emp.dist,method = "complete")
emp.ave <- hclust(emp.dist,method = "average")

par(mfrow=c(1,3))
plot(emp.min)
plot(emp.max)
plot(emp.ave)

Min4 <- cutree(emp.min,k=3)
split(lab,Min4)

Min4 <- cutree(emp.min,k=3)


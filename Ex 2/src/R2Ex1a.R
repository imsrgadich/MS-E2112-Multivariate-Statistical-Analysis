setwd('/home/imsrgadich/Documents/OneDrive/Aalto/Sem3&4/MSA/Ex 2')
library("mvtnorm", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")
set.seed(143)

mydata = read.table("DECATHLON.txt", header = TRUE)

exp_val <-matrix(c(4,7), nrow = 2, ncol =1)
cov <- matrix(c(10,6,6,8), nrow = 2, ncol =2)

x <- rmvnorm(n=2000, mean=exp_val, sigma=cov)
plot(x)

x.PCA <- princomp(x,cor=FALSE)

xModPCA <- x.PCA$scores

plot(xModPCA)

xPrinVec <- x.PCA$loadings

plot(x,col="red")
lines(c(-10, 15),c(-8.455497382, 12.68324607),type="l",col="black")
## uSE arrow
# 0,845549738
## [-10, −8,455497382]
## [15, 12,68324607]

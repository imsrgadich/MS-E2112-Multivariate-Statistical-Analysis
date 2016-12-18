setwd('/home/imsrgadich/Documents/OneDrive/Aalto/Sem2/MSA/Ex 4')

n <- 200
p = 2

D1 = matrix(rnorm(n*p), ncol = p)
D2 = matrix(rt(n*p, df=5), ncol = p)
D31 <- rweibull(n,shape =1,scale =2)
D32 <- rgamma(n,shape = 2,scale =1)

D3 <- cbind(D31,D32)

mcd1 <- CovMcd(D1,alpha=0.5)
cov1 <- cov(D1)

mcd1@cov


mcd1 <- CovMcd(D2,alpha=0.5)
cov1 <- cov(D2)

mcd2@cov


mcd2@cov
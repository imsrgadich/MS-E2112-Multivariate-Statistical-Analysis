library("mvtnorm", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")
set.seed(123)

exp_val <-matrix(c(3,1), nrow = 2, ncol =1)
cov <- matrix(c(4,1,1,2), nrow = 2, ncol =2)

x <- rmvnorm(n=100, mean=exp_val, sigma=cov)
plot(x)

#Problem 1 c

sprintf("The mean of the observations is:")
mean_sample <- colMeans(x)
mean_sample

sprintf("The covaraince of the observations is:")
cov_sample <- cov(x)

eig <- eigen(cov_sample)

sprintf("The eigenvalues of the sample covariance is:")

eig$values

sprintf("The eigenvectors of the sample covariance is:")

eig$vectors

sprintf("To show that sum of eigen values is equal to trace of covariance matrix")
sprintf("The sum of diagnol elements is")
sum(diag(cov_sample))
sprintf("The sum of eigenvalues is")
sum(eig$values)

sprintf("To show that product of eigen values is ")








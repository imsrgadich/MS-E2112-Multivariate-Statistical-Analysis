cov2corr <- function(covariance) {
  
  invSqrtCov = sqrt(diag(covariance))
  
  return(covariance / (invSqrtCov %*% t(invSqrtCov)))
  
}
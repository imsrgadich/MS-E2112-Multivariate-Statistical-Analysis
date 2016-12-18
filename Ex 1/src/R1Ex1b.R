squareRootInv <- function(matrix){
  
  eigValues = eigen(matrix)$values
  
  eigVectors = eigen(matrix)$vectors
  
  modValues = 1/diag(sqrt(eigValues))
  
  modValues[modValues == Inf] <- 0
  
  return(eigVectors %*% modValues %*% t(eigVectors))
  
}


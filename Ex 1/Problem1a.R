A <- matrix(c(2,-2,5,1,7,-8,5,0,-1), nrow = 3, ncol =3 )
x <- matrix(c(8,-4,2), nrow = 1, ncol =3 )
b <- matrix(c(3,10,-19), nrow = 1, ncol =3 )

y <- x %*% solve(A) + b

sprintf("The affine transformation y is :")
y



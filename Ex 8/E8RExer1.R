setwd("~/Documents/OneDrive/Aalto/Sem2/MSA/Ex 8")

dec_data <- read.table("DECATHLON.txt",header=T,sep="\t")

Y <- dec_data[,c(3,4,5,6,7,8,9,10,11,12)] # X = (Price,Value)
X <- dec_data[,c(13,14)] # Y = (Economy,Service, Desing,Sport,Safety,
#                               Easy.h)
# (a)
#How many pairs of canonical variables can we obtain?
# Two pairs, since we need orthogonal u_i and v_i vectors
# and X has two components (2<6)
XY <- as.matrix(cbind(X,Y))

R <- cov(XY)
R11 <- R[1:2,1:2]
R22 <- R[3:12,3:12]
R21 <- R[3:12,1:2]
R12 <- R[1:2,3:12]
R11.inv <- solve(R11)
R22.inv <- solve(R22)

#Non-zero eigenvalues of M1 and M2 are the same
M1 <- R11.inv %*% R12 %*% R22.inv %*% R21
M2 <- R22.inv %*% R21 %*% R11.inv %*% R12


# Note that the eigenvectors are unique up to sings
# (sometimes R calculates the eigenvectors multiplied with -1)
va1 <- eigen(M1)$vectors[,1]
va2 <- eigen(M1)$vectors[,2]

vb1 <- eigen(M2)$vectors[,1]
vb2 <- eigen(M2)$vectors[,2]
# a1 = alpha1 , b1 = beta1
# Correct scaling: Whitening of data
a1 <- -va1/sqrt(va1%*%R11%*%va1)
a2 <- -va2/sqrt(va2%*%R11%*%va2)
b1 <- vb1/sqrt(vb1%*%R22%*%vb1)
b2 <- vb2/sqrt(vb2%*%R22%*%vb2)

fii1 <- XY[,1:2]%*%a1
fii2 <- XY[,1:2]%*%a2
eta1 <- XY[,3:12]%*%b1
eta2 <- XY[,3:12]%*%b2

round(cor(cbind(fii1,fii2,Re(eta1),Re(eta2))),2)

#canonical correlations
# g1 = 0.78
# g2 = -0.49

# Value = "Loss of value" = "How fast the value goes down"??

# u1 <- 0.0574*Height + 0.1079*Weight
# v1 <- -0.0037*R100m - 0.0007*Long Jump + 0.0085*Shot Put - 0.0011*High Jump - 0.0004*R400m
#       + 0.003*Hurdles + 0.0061*Discus Throw -0.0040*Pole Vault -0.0029*Javelin -0.0007*R1500

## Stand and deliver games(like shot put and discus throw) requires more physical strength which are mainly
## contributed by physical characteristics 

plot(fii1,Re(eta1),xlab="'Physical Strength'",ylab="'Stand and deliver games'",pch="")
text(fii1,Re(eta1),labels=dec_data$NAME)



# u2 <- -0.3710*Height + 0.2573*Weight
# v2 <- -0.0067*R100m -0.0017*LongJump - 0.0052*Shot Put - 0.0014*High Jump + 0.0112*R400m
#     + 0.0110*Hurdles + 0.0118*DiscusThrow - 0.0014*Pole Vault + 0.0019*Javelin + 0.0082*R1500

# The second pair of canonical variables provides more insight into the relation ship between the two sets
# of variables. u2 has low negative values for cars with good marks both in price and value, e.g., VW and Opel.
# On the right hand side, we should see cars with bad marks in these two variables such as Ferrari and Wartburg.
# The canonical variable v2 consists mainly of variables economy, service and easy handling. The position
# of cars is displayed in the plot below

plot(fii2,Re(eta2),xlab="'Stamina'",ylab="'Running Games'",pch="")
text(fii2,Re(eta2),labels=dec_data$NAME)


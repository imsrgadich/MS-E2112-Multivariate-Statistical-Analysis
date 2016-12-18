setwd("//home.org.aalto.fi/gadichs1/data/Desktop/P3_4/MSA/Ex 8")

car <- read.table('CAR.txt',header=T,sep = '\t')

View(car)

# X = (PRICE,VALUE)
# Y = (ECONOMY,SERVICE,DESIGN,SPORT,SAFETY,EASY H.)

X <- car[,c(6,5)]
Y <- car[,c(3,4,7,8,9,10)]

# (a) How many pairs of canonical variables can we obtain?
# 2

# (b)

dat <- cbind(X,Y)

R <- cor(dat)
R

R11 <- R[1:2,1:2]
R22 <- R[3:8,3:8]
R21 <- R[3:8,1:2]
R12 <- R[1:2,3:8]

R11.inv <- solve(R11)
R22.inv <- solve(R22)

M1 <- R11.inv %*% R12 %*% R22.inv %*% R21
M2 <- R22.inv %*% R21 %*% R11.inv %*% R12

# We are taking only two eigen vectors because other the rest of eigen values are zero.
a1 <- eigen(M1)$vectors[,1]
a2 <- eigen(M1)$vectors[,2]

b1 <- eigen(M2)$vectors[,1]
b2 <- eigen(M2)$vectors[,2]

#Whitening
round(a1 <- a1/sqrt(a1 %*% R11 %*% a1),2)
round(a1 <- a2/sqrt(a2 %*% R11 %*% a2),2)
round(a1 <- b1/sqrt(b1 %*% R22 %*% b1),2)
round(b2 <- b2/sqrt(b2 %*% R22 %*% b2),2)

data.std <- apply(dat,2,scale)
data.std

colSums(data.std)

fii1 <- data.std[,1:2] %*% a1
fii2 <- data.std[,1:2] %*% a2

eta1 <- data.std[,3:8] %*% b1
eta2 <- data.std[,3:8] %*% b2

# u1 = 0.37 Price - 0.67 value
# v1 = 0.38 Economy - 0.17 Service + 0 Design -0.47 Sport -0.24 Safety - 0.24 Easy.h

round(cor(cbind(fii1,fii2,eta1,eta2)),2)

round(sqrt(eigen(M1)$values),2)
round(sqrt(eigen(M2)$values),2)

# canonical correlations are 
# g1 = 0.98
# g2 = 0.91

# The relationship seem to be strong 

plot(fii1,eta1,xlab="Interpretation of u1",ylab="Interpretation of v1",pch="")
text(fii1,eta1,labels=car$Type)

plot(fii2,eta2,xlab="Interpretation of u1",ylab="Interpretation of v1",pch="")
text(fii2,eta2,labels=car$Type)




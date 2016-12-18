car <- read.table("CAR.txt",header=T,sep="\t")

X <- car[,c(6,5)] # X = (Price,Value)
Y <- car[,c(3,4,7:10)] # Y = (Economy,Service, Desing,Sport,Safety,
#                               Easy.h)
# (a)
#How many pairs of canonical variables can we obtain?
# Two pairs, since we need orthogonal u_i and v_i vectors
# and X has two components (2<6)
XY <- as.matrix(cbind(X,Y))

R <- cov(XY)
R11 <- R[1:2,1:2]
R22 <- R[3:8,3:8]
R21 <- R[3:8,1:2]
R12 <- R[1:2,3:8]
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
# Correct scaling
a1 <- va1/sqrt(va1%*%R11%*%va1)
a2 <- va2/sqrt(va2%*%R11%*%va2)
b1 <- vb1/sqrt(vb1%*%R22%*%vb1)
b2 <- vb2/sqrt(vb2%*%R22%*%vb2)

fii1 <- XY[,1:2]%*%a1
fii2 <- XY[,1:2]%*%a2
eta1 <- XY[,3:8]%*%b1
eta2 <- XY[,3:8]%*%b2

round(cor(cbind(fii1,fii2,Re(eta1),Re(eta2))),2)

#canonical correlations
# g1 = 0.98
# g2 = 0.91
# Value = "Loss of value" = "How fast the value goes down" 

# u1 <- 0.32Price - 0.62Value
# v1 <- 0.43Economy - 0.21 Service +0Desing - 0.47Sport - 0.22Safety 
#             -0.4 Easy.h

# Note that 1=very good, 6=very bad

# From the first pair of canonical variables (u1, v1), we see that Price is positively related to Economy,
# and negatively related to the remaining characteristics of a car (service, sportiness, safety and easy
#  handling). The variable Value is negatively related to Economy and positively related to the other
# characteristics.
# The canonical variable u1 can be interpreted as a value index of the car. On the one side, we observe
# cars with good (low) price and bad (high) appreciation of value such as Trabant and Wartburg and on
# the other side, we see cars with high price and good (low) appreciation of value such as BMW, Jaguar,
# Ferrari and Mercedes. Similarly, v1 can be interpreted as a quality index consisting of variables such as
# service, safety and sport. The canonical variable v1 has cars with bad economy and good service, sport, safety
# and easy handling on the other side such as BMW, Jaguar, Ferrari and Mercedes and cars with good economy and
# bad service, sport, safety and easy handling on the other side. 

# u1 mainly consists of Value and 
# v1 mainly consists of Economy, Sport and Easy handling

#The value and quality indeces are highly correlated with the canonical correlation
# coefficient 0.98. This can be seen in the following plot:
  

plot(fii1,eta1,xlab="'Value index' of the car",
     ylab="'Quality' of the car",pch="")
text(fii1,eta1,labels=car$Type)



# u2 <- -1.41Price - 1.42 Value  ???
# v2 <- -0.46Economy -0.70Service  + 0.06Desing  + 0.00Sport +  0.30Safety  -1.01Easy.h ???

# The second pair of canonical variables provides more insight into the relation ship between the two sets
# of variables. u2 has low negative values for cars with good marks both in price and value, e.g., VW and Opel.
# On the right hand side, we should see cars with bad marks in these two variables such as Ferrari and Wartburg.
# The canonical variable v2 consists mainly of variables economy, service and easy handling. The position
# of cars is displayed in the plot below

plot(fii2,eta2,xlab="u2",
     ylab="v2",pch="")
text(fii2,eta2,labels=car$Type)



#The other parts were not discussed in the exercise session

## M1 or M2 not full rank matrix. Why?


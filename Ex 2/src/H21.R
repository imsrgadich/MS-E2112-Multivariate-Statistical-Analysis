setwd("~/Documents/OneDrive/Aalto/Sem2/MSA/Ex 2")

data <- read.table("DECATHLON.txt",header=T)

View(data)

colnames(data)

help(princomp)

DEC <- data[,-c(1,2,13,14)]
dim(data)

rownames(DEC) <- data[,1]

DEC.PCA <- princomp(DEC,cor=FALSE)


stdev <- DEC.PCA$sdev

stdev^2

DEC.cov <- cov(DEC)
DEC_cov_evalues <- eigen(DEC.cov)$values

View(DEC.PCA$loadings)

DEC.PCA$loadings[1,1]


DEC.PCA$scores[1:9,1:9]


names(DEC.PCA)

DEC.PCA$scale

DEC.PCA$call

summary(DEC.PCA)

pairs(DEC.PCA$scores)

plot(DEC.PCA$scores[,1],type="l")


sum(DEC_cov_evalues[1:3])/sum(DEC_cov_evalues) 

scores <- DEC.PCA$scores

cov(scores)

diag(cov(scores))

colMeans(scores)

DEC.PCA$loadings
# High positive loadings with running (1500m and 400m)
# High jump is mysterious, maybe related to jumping power?

#Interpretation of the first PC (principal component):
# Strength (weakness), here large negative values mean that the sport in question
# requires strength. However, atheletes with a high body muscle mass are bad in
# "long" running distances, when comparing to other decathletes.

#First PC explains particularly well the behavior of shot_puck, R1500m and 
# Discuss_Throw. 

# PC2 = speed... 

# Uncorrelatedness not equal to indepence 

# PC3, PC4 = technique 1 and 2, a little hard to interpret

biplot(DEC.PCA)

biplot(DEC.PCA,choices=c(1,6),xlabs=rep("",48))

summary(DEC.PCA)


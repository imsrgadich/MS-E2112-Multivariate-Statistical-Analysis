setwd("//home.org.aalto.fi/gadichs1/data/Desktop/P3_4/MSA/Ex 10")

BANK <- read.table("BANK.txt",header = T,sep="\t")

set.seed(425)

k.mean <- kmeans(BANK[,-1],center=2)

table(k.mean$cluster,BANK[,1])

par(mfrow=c(1,1))

#par(mfrow=c(1,2))

clusplot(BANK[,-1],k.mean$cluster,color=T,shade=T)
clusplot(BANK[,-1],BANK[,1],color=T,shade=T)


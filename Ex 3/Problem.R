setwd('/home/imsrgadich/Documents/OneDrive/Aalto/Sem3&4/MSA/Ex 3')

DEC <- read.table("DECATHLON.txt", header =T)
colnames(DEC)

modDEC <- DEC[,-c(1,2,13,14)] 

modDEC.PCA <- princomp(modDEC,cor = T)

screeplot((modDEC.PCA))

eig.val <- modDEC.PCA$sdev^2

eig.val.percentage <- eig.val/sum(eig.val)

barplot(eig.val.percentage,names.arg = 1:length(eig.val),
        main = "VARIANCES",
        xlab = "Principal components",
        ylab = "Percentage of variance",
        col = "steelblue")

##points and lines commands are important

lines(x=1:length(eig.val),eig.val.percentage, type = "b", pch=19, col = "red")

summary(modDEC.PCA)

#install.packages("corrgram")

colMeans(modDEC.PCA$scores)
cov(modDEC.PCA$scores)

rownames(modDEC) <- DEC[,1] 

outlier <- rep(0,10)

#outlier[1] <- 0

modDEC_outlier <- rbind(modDEC,outlier)

modDEC_outlier.PCA <- princomp(modDEC_outlier.PCA, corr = T)

DEC2 <- read.table("DECATHLON2.txt", header =T, sep = ",")

# install library ade4
library(ade4)
library(corrgram)
# dudi.pca and s.corcircle

dec2.active <- DEC2[1:23,1:10]

corrgram(dec2.active)

pca.res <- dudi.pca(dec2.active,scale = T, center = T, scannf = F, nf =5)
names(pca.res)

s.corcircle(pca.res$co)

quali.sup <- as.factor(DEC2[1:23,13])

names(quali.sup)

s.class(pca.res$li,fac=quali.sup,xax=1,yax=2,col=c("red","blue"))

scatter(pca.res,posieig = "none",clab.row=0)

quali.sup2 <- quali.sup

levels(quali.sup2)[3] <- "Unknown"

quali.sup2[24:27] <- "Unknown"

ind.sup <- DEC2[24:27,1:10]

ind.sup.pca <- suprow(pca.res,ind.sup)

li <- rbind(pca.res$li,ind.sup.pca$lisup[,1:5])

li
res <- scatter(pca.res,clab.row = 0,posieig = "none")


s.class(li,fac=quali.sup2,col=c("red","blue","green"),
        add.plot = T,cstar = 0,cellipse = 0,clabel = 0)

identify(x=ind.sup.pca$lisup[,1],y= ind.sup.pca$lisup[,2],
         labels = rownames(ind.sup.pca$lisup))

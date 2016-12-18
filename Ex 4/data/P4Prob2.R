setwd('/home/imsrgadich/Documents/OneDrive/Aalto/Sem2/MSA/Ex 4')
D <- read.table("wood.txt",header = T)

dim(D)
library(rrcov)

plot(D)

covOriginalData = cov(D)
covMCDData = CovMcd(Data,alpha=1) # Get covariance similar to cov(data)
covMCDData = CovMcd(D,alpha=1/2)

covMCDData@cov #To get the covariance from MCD 
covMCDData@raw.center #Mean of the robust data


#Question d

help("mahalanobis")

origData.maha <- mahalanobis(D,center=colMeans(D),cov=covOriginalData)
mcdData.maha <- mahalanobis(D,center=covMCDData@center,cov=covMCDData@cov)


## the square of the maha distance follows chi^2 distribution 
## with p degress of freedom, p= # of variables (5 here) 
# assuming that the original data is normally distributed.

help(abline)
qqplot(qchisq(ppoints(20),df=5),origData.maha, main = "Using sample covariance matrix")
abline(0,1,col=2) ## or col ="red"

qqplot(qchisq(ppoints(20),df=5),mcdData.maha, main = "Using sample covariance matrix")
abline(0, 1, col = 2)

# Get labels of outliers data

tmp = qqplot(qchisq(ppoints(20),df=5),origData.maha, plot=F)
lab = (1:nrow(D))[origData.maha]

tmp2 = qqplot(qchisq(ppoints(20),df=5),mcdData.maha, plot=F)
lab = (1:nrow(D))[mcdData.maha]

identify(tmp$x,y = tmp$y , labels = lab,plot =T)

####################################
par(mfrow=(c(2,1))) # like subplot in MATLAB

plot(c(1,nrow(D)),range(sqrt(c(origData.maha,mcdData.maha))),type ='n',
     xlab="Observation", ylab="Mahalanobis-Distance")

points(c(1:nrow(D),sqrt(origData.maha),col="blue"))

points(c(1:nrow(D),sqrt(mcdData.maha),col="red"))

abline(h=sqrt(qchisq(0.975,df=5)),col="black")

legend(x=1,y=9,col=c("blue","red"),pch=c(1,16),cex=0.8,
       legend= c("Classical Estimation","MCD Estimation"))
dev.off()

PCA1 = princomp(D,cor=T)
summary(PCA1)

PCA2 = PcaCov(D,scale=T,center=T)  ## Using rrcov package using robust covariance estimator.

screeplot(PCA1)
screeplot(PCA2)

PCA1$loadings
PCA2@loadings

pairs(PCA1$scores)
pairs(PCA2@scores) 





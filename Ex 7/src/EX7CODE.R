#setwd("//home.org.aalto.fi/lietzen1/data/Desktop/Monim2016")

# Read the data
tea <- read.table("TEA.txt",header=T,sep="\t")
dim(tea)
View(tea)




# A nice for loop to present the original variables
# barplots would also be a nice option here
# (note that barplot is not the same thing as histogram)

for(i in 1:dim(tea)[2]){

tmp <- table(tea[,i])

lab <- round(100*tmp/sum(tmp),1)

pielabels <- paste(lab,"%",sep="")

cols <- c("black","grey","green","red")

title <- paste("Question",i,sep=" ")
pie(tmp, main=title,col=cols,labels=pielabels,cex=0.8)

legend("topleft",names(lab),fill=cols,horiz=F,cex=0.5)
}


#install.packages("ca")
#install.packages("ggplot2")

library(ca)
library(ggplot2)

help(mjca)

tea.mca <- mjca(tea,lambda="indicator")

names(tea.mca)

tea.mca$factors
tea.mca$levels.n
tea.mca$sv^2

(tea.mca$sv[1]^2 + tea.mca$sv[2]^2) / sum(tea.mca$sv^2)


#Points allows us to add the observations to the plot
plot(tea.mca)
points(tea.mca$rowcoord)


cats <- apply(tea,2, function(x) nlevels(as.factor(x)) )

tea.vars <- data.frame(tea.mca$colcoord,Variable= rep(names(cats),cats))


tea.obs <- data.frame(tea.mca$rowcoord)


rownames(tea.vars) <- tea.mca$levelnames



ggplot()+
  geom_text(data=tea.vars,aes(x=X1,y=X2,colour = Variable,label=rownames(tea.vars)))+
  geom_text(data = tea.obs,aes(x=X1,y=X2,label=rownames(tea.obs)))+
  geom_hline(yintercept=0,colour="gray70")+
  geom_vline(xintercept=0,colour="gray70")+
  ggtitle("MCA plot")
  



setwd("~/Documents/OneDrive/Aalto/Sem2/MSA/Ex 7")


tea <- read.table('TEA.txt',header=T,sep='\t')

dim(tea)

View(tea)

help("pie")

table1 <- table(tea$Tea)

pie(table1)

lab <- round(100*table1/sum(table1),1)

pielabels <- paste(lab,"%",sep="")

cols <- c("black","grey","green")

pie(table1,main="What kind of tea do you drink?",col=cols,labels=pielabels,cex=0.8)

legend("topleft",names(lab),cex=0.8,fill=cols)

library(ca)
library(ggplot2)

help(mjca)

tea.mca <- mjca(tea,lambda="indicator")

tea.mca$factors ## check help for answers
tea.mca$levels.n
tea.mca$sv^2  ## square to get eigen values

((tea.mca$sv[1]^2 + tea.mca$sv[2]^2) / sum(tea.mca$sv^2))

plot(tea.mca)

cats <- apply(tea,2,function(x) nlevels(as.factor(x)))
                                           
tea.vars <- data.frame(tea.mca$colcoord,Variable=rep(names(cats),cats))

tea.vars

rownames(tea.vars) <- tea.mca$levelnames

help("ggplot")
ggplot(data = tea.vars,
       aes(x=X1,y=X2,label=rownames(tea.vars)))+ 
      geom_hline(yintercept = 0,colour="gray70")+
      geom_vline(xintercept = 0,colour="gray70")+
      geom_text(aes(colour=Variable))+
  ggtitle("MCA plot")

## ggplot multiplot


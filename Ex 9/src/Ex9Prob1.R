setwd("//home.org.aalto.fi/gadichs1/data/Desktop/P3_4/MSA/EX 9")

data(iris) 
View(iris)

#install.packages('MASS')

library('MASS')

a <- lda(iris)

help('lda')


data <- iris[51:150,]

data[,5] <- factor(data[,5])


data.lda <- lda(Species~.,data=data)

names(data.lda)

data.lda

# this is vector 'a'
data.lda$scaling

data.lda$means

# new data

newdata<- data.frame(Sepal.Length=6,Sepal.Width = 3,Petal.Length=4,Petal.Width=1)

predict(data.lda,newdata=newdata)$class

data.cv <- lda(Species~.,data=data,CV=T)

data.cv

result <- data.frame(est=data.cv$class,truth=data[,5])

tab <- table(result)

tab

1-sum(diag(tab))/nrow(data)

# 3 misclassifications in 100 data.

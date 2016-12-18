setwd("//home.org.aalto.fi/gadichs1/data/Desktop/P3_4/MSA/EX 6")

data <- read.table("SCIENCEDOCTORATES.txt",header = T, row.names = 1)

dim(data)

S = data[-13,-9]

help(ca)

S.ca = ca(S,nd=NA)

summary(S.ca)
plot(S.ca,arrows=c(T,T))

plot(S.ca)

help("chisq.test")
              
              # H0 : column and row are independent    
chisq.test(S) # Rejct H0, p-value 


plot(S.ca,arrows=c(T,T))

plot3d.ca(S.ca)

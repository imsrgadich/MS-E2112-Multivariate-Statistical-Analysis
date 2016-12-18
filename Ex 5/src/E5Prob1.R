setwd("//home.org.aalto.fi/gadichs1/data/Desktop/P3_4/MSA/EX5")

data <- read.table("SCIENCEDOCTORATES.txt",header = T, row.names = 1)


dim(data)
class()

# prop table wants a matrix
D <- as.matrix(data)

prop.table(D,1) #row profile 
prop.table(D,2) #col profile 

rowSums(prop.table(D,1)) #  All the rows sholud add to 1

colSums(prop.table(D,2)) # All the columns should add to 1

margin.table(D,1)
data[,9]

margin.table(D,2)
data[13,]

margin.table(D)
v1 <- margin.table(D,1)
v2 <- margin.table(D,2)

n1 = length(v1)
n2 = length(v2)

V1 = matrix(v1,ncol = 1)
V2 = matrix(v2,ncol=n2)

E = V1 %*% V2 / sum(D)

AR.matrix <- D/E # D = original data (number of observations)
                 # E = expected number of observations under independence

view(AR.matrix)
#values near 1 : science and year are independent
#values less 1 : science and year are not attracted i.e., repulsed. Science is less frequent in that year
#values more 1 : science and year are attracted. science was more studied then.



# form the matrix of theoretical frequencies using margin.table(D,1) and 
# margin.table(D,2) (outer product)

#form the atttraction repulsion table

round(AR.matrix,2)

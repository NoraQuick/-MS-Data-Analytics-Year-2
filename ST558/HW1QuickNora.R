# ST 558: Multivariate Analytics
# Homework 1
# Nora Quick

# Import Iris csv
iris <- read.csv('IrisData.csv')
head(iris)

# Problem 1
# --------------
# Part (a)
iris.samplemeans <- rep(0, 4)

for(j in 1:4){
  iris.samplemeans[j] <- mean(iris[,j])
}

iris.samplemeans

# Problem 1
# ---------------
# Part (b)
iris.samplecov <- cov(iris[,1:4], use='pairwise.complete.obs')
iris.samplecov

iris2 <- iris
#iris2

iris2$Sepal.Width <- iris2$Petal.Width / 2.54

iris2.samplecov <- cov(iris2[,1:4], use='pairwise.complete.obs')
iris2.samplecov

# Problem 1
# ---------------
# Part (c)
iris.samplecor <- cor(iris[,1:4], use='pairwise.complete.obs')
iris.samplecor

iris2 <- iris
#iris2

iris2$Petal.Width <- iris2$Petal.Width / 2.54

iris2.samplecor <- cor(iris2[,1:4], use='pairwise.complete.obs')
iris2.samplecor

# Problem 1
# --------------
# Part (d)
pairs(iris[,1:4], col=rainbow(13)[factor(iris$Type)])

# Problem 1
# --------------
# Part (e)
matZ <- c(0.3, 0.1, 0.4, 0.2)
#matZ

newIris <- iris[,1:4]
head(newIris)

linComb <- as.matrix(newIris) %*% matZ
#linComb

mean(linComb)



# Import Cubit csv
cubit <- read.csv('CubitData.csv')
head(cubit)

# Problem 2
# ---------------
# Part (a)
cubit.samplemeans <- rep(0, 2)

for(j in 1:2){
  cubit.samplemeans[j] <- mean(cubit[,j])
}

cubit.samplemeans

# Problem 2
# ---------------
# Part (b)
cubit.samlecov <- cov(cubit[,1:2], use='pairwise.complete.obs')
cubit.samlecov

# Problem 2
# ----------------
# Part (c)
cubit.eig <- eigen(cubit.samlecov)
cubit.eig

# Problem 2
# ----------------
# Part (d)
cubit.eig$vec
cubit.eig$val

# Problem 2
# ----------------
# Part (e)
one <- (cubit.samplemeans[1]+cubit.eig$vec[1,1])
two <- (cubit.samplemeans[2]+cubit.eig$vec[2,1])

x1 <- cubit.samplemeans
y1 <- c(one, two)
                
plot(cubit$cubit, cubit$height, cex = 1, pch = 19, 
     xlab ="cubit", ylab ="height", col ="black")

lines(x1, y1, col = "red")


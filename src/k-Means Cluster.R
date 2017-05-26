library(ProjectTemplate)
load.project()

#get wine data
data(wine, package="rattle")

#EDA of wine data
head(wine)
str(wine)
summary(wine)

#scale data so that all numeric variables are normalized with mean of 0
data.train=scale(wine[-1])
summary(data.train)

#method 1 to identify suggested number of clusters
library(NbClust)
nc1<-NbClust(data.train, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1, 1))
barplot(table(nc1$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
nc1Est<-3

#method 2
# Determine number of clusters
wss <- (nrow(data.train)-1)*sum(apply(data.train,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data.train, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
nc2Est<-3

#method 3
library(fpc)
library(cluster)
pamk.best <- pamk(data.train)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
par(mfrow=c(2,2))
#plotting error not supported in later versioned packages
## clusplot(pam(data.train, pamk.best$nc), color=TRUE)

#method 3b:
library(fpc)
asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- pam(data.train, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")



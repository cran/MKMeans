\name{MKMeans-package}
\alias{MKMeans-package}
\docType{package}
\title{ Modern K-Means (MKMeans) Clustering.}
\description{ It's a Modern K-Means clustering algorithm which works for data of any number of dimensions, has no limit with the number of clusters expected,
and can start with any initial cluster centers.
}  

\details{
\tabular{ll}{
Package: \tab MKMeans\cr
Type: \tab Package\cr
Version: \tab 3.3\cr
Date: \tab 2025-08-27\cr
License: \tab GPL-2\cr
}
 }
\author{ Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang  }
\references{ Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang.(2025)  MKMeans: A Modern K-Means Clustering Algorithm. technical report
}

\keyword{package }

\examples{
# Example 1:

# Generate 20 bivarate samples
x<-rnorm(20,0,1)
y<-rnorm(20,1,1)
data.test<-cbind(x,y)

# Conduct MKMeans analysis with K=3 and taking the first 3 samples as initial cluster centers 
Res<-MKMeans(data.test,3,1,iteration=1000,type=1)
Ress<-Res
names(Ress@Classes[[1]])<-rep("red",length(Res@Classes[[1]]))
names(Ress@Classes[[2]])<-rep("blue",length(Res@Classes[[2]]))
names(Ress@Classes[[3]])<-rep("green",length(Res@Classes[[3]]))
Cols<-names(sort(c(Ress@Classes[[1]],Ress@Classes[[2]],Ress@Classes[[3]])))
plot(x,y,type="p",col=Cols,lwd=2)
points(Res@Centers,pch=15,col=c("red","blue","green"))  

# Example 2:
library(MASS)
# Generate 10 bivariate normal samples
mu1 <- c(0, 0)          
sigma1 <- matrix(c(1, 0.5, 0.5, 1), nrow=2)  
SP1 <- mvrnorm(n=10, mu=mu1, Sigma=sigma1)

# Generate another 10 bivariate normal samples
mu2<-c(1,1)
sigma2<-matrix(c(1,0,0,1),nrow=2)
SP2<-mvrnorm(n=10,mu=mu2,Sigma=sigma2)

# Generate 10 more new bivariate normal samples
mu3<-c(2,2)
sigma3<-matrix(c(1,0.5,0.5,1),nrow=2)
SP3<-mvrnorm(n=10,mu=mu3,Sigma=sigma3)

# Combine the three groups of bivariate normal samples
data<-rbind(SP1,SP2,SP3)

# Conduct MKMeans analysis with K=4 and randomly picking four samples as initial cluster centers
Res<-MKMeans(data,4,data[sample(1:30,4),],iteration=1000,type=1)
names(Res@Classes[[1]])<-rep("red",length(Res@Classes[[1]]))
names(Res@Classes[[2]])<-rep("blue",length(Res@Classes[[2]]))
names(Res@Classes[[3]])<-rep("green",length(Res@Classes[[3]]))
names(Res@Classes[[4]])<-rep("black",length(Res@Classes[[4]]))
Cols<-names(sort(c(Res@Classes[[1]],Res@Classes[[2]],Res@Classes[[3]],Res@Classes[[4]])))
plot(data[,1],data[,2],type="p",pch=19,col=Cols,lwd=2,xlab="",ylab="")
points(Res@Centers,pch=5,col=c("red","blue","green","black"))

}


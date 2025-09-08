\name{MKMeans-package}
\alias{MKMeans-package}
\docType{package}
\title{ Modern K-Means (MKMeans) Clustering.}
\description{ It's a Modern K-Means clustering algorithm which works for data of any number of dimensions, has no limit with the number of clusters expected, offers both methods with and without initial cluster centers, and can start with any initial cluster centers for the method with initial cluster centers.
}  

\details{
\tabular{ll}{
Package: \tab MKMeans\cr
Type: \tab Package\cr
Version: \tab 3.4.4\cr
Date: \tab 2025-09-08\cr
License: \tab GPL-2\cr
}
 }
\author{ Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang  }
\references{ Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang.(2025)  MKMeans: A Modern K-Means Clustering Algorithm. technical report in preparation
}

\keyword{package }

\examples{

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

# Conduct MKMeans analysis with K=3 by the "YY" method
Res<-MKMeans(data,3,method="YY",iteration=1000,type=1)
names(Res@Classes[[1]])<-rep("red",length(Res@Classes[[1]]))
names(Res@Classes[[2]])<-rep("blue",length(Res@Classes[[2]]))
names(Res@Classes[[3]])<-rep("green",length(Res@Classes[[3]]))
Cols<-names(sort(c(Res@Classes[[1]],Res@Classes[[2]],Res@Classes[[3]])))
plot(data[,1],data[,2],type="p",pch=19,col=Cols,lwd=2,xlab=paste("Total SSE = ",
     round(Res@SSE[length(Res@SSE)],2),sep=""),ylab="",
     main="MKMeans Clustering Results by the 'YY' method")
points(Res@Centers,pch=5,col=c("red","blue","green")) 

#  Compare the clustering results with the original samples 
par(mfrow=c(1,2))
plot(data[,1],data[,2],type="p",pch=19,col=rep(c("sky blue","orange","purple"),rep(10,3)),
     lwd=2,xlab="",ylab="",main="Original Data")
plot(data[,1],data[,2],type="p",pch=19,col=Cols,lwd=2,xlab=paste("Total SSE = ",
     round(Res@SSE[length(Res@SSE)],2),sep=""),ylab="",
     main="MKMeans Clustering Results by the 'YY' method")
points(Res@Centers,pch=5,col=c("red","blue","green")) 

# Conduct MKMeans analysis with K=3 and randomly picking 3 samples as initial cluster centers
Res2<-MKMeans(data,3,method="initial",initial=data[sample(1:nrow(data),3),],iteration=1000,type=1)
names(Res2@Classes[[1]])<-rep("red",length(Res2@Classes[[1]]))
names(Res2@Classes[[2]])<-rep("blue",length(Res2@Classes[[2]]))
names(Res2@Classes[[3]])<-rep("green",length(Res2@Classes[[3]]))
Cols2<-names(sort(c(Res2@Classes[[1]],Res2@Classes[[2]],Res2@Classes[[3]])))
plot(data[,1],data[,2],type="p",pch=19,col=Cols2,lwd=2,xlab=paste("Total SSE = ",
     round(Res2@SSE[length(Res2@SSE)],2),sep=""),ylab="",
     main="MKMeans Clustering Results by the 'initial' method")
points(Res2@Centers,pch=5,col=c("red","blue","green"))

#  Compare the clustering results by the "YY" method and by the "initial" method
par(mfrow=c(1,2))
plot(data[,1],data[,2],type="p",pch=19,col=Cols,lwd=2,xlab=paste("Total SSE = ",
     round(Res@SSE[length(Res@SSE)],2),sep=""),ylab="",
     main="MKMeans Clustering Results by the 'YY' method")
points(Res@Centers,pch=5,col=c("red","blue","green")) 
plot(data[,1],data[,2],type="p",pch=19,col=Cols2,lwd=2,xlab=paste("Total SSE = ",
     round(Res2@SSE[length(Res2@SSE)],2),sep=""),ylab="",
     main="MKMeans Clustering Results by the 'initial' method")
points(Res2@Centers,pch=5,col=c("red","blue","green")) 

}


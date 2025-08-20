\name{MKMeans}
\alias{MKMeans}
\title{Modern K-Means clustering.}
\description{
    It's a Modern K-Means clustering algorithm which works for data of any number of dimensions, has no limit with the number of clusters expected, and can start with any initial cluster centers.
}  
\usage{
MKMeans(data, K, initial, iteration, tol, type)
}
\arguments{
  \item{data}{ Numeric. An observation matrix with each row being an oberservation.}
  \item{K}{ Integer.  The number of clusters expected.}
  \item{initial}{ Numeric. Either the selected initial center matrix with each row being an observation, or 1 for the first K rows of the data matrix being the intial center.     }
  \item{iteration}{ Integer. The number of the most iterations wanted for the clustering process. }
  \item{tol}{ Numeric. The minimum acceptable percentage of stable observations to stop the clustering process, basically greater than 0.5 to guarantee the value of the results. }
  \item{type}{ Integer. The type of distance between observations. 1 for Euclidean distance. 2 for Manhattan distance. 3 for maximum deviation among dimensions.}
}

\value{An object of class MKMean.}
\author{Yarong Yang}
\references{Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang.(2025) MKMeans: A Modern K-Means Clustering Algorithm. technical report
}
\examples{

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

# Conduct MKMeans analysis with K=3 and randomly picking three samples as initial cluster centers
Res<-MKMeans(data,3,data[sample(1:30,3),],iteration=1000,tol=.95,type=1)
names(Res@Classes[[1]])<-rep("red",length(Res@Classes[[1]]))
names(Res@Classes[[2]])<-rep("blue",length(Res@Classes[[2]]))
names(Res@Classes[[3]])<-rep("green",length(Res@Classes[[3]]))
Cols<-names(sort(c(Res@Classes[[1]],Res@Classes[[2]],Res@Classes[[3]])))
plot(data[,1],data[,2],type="p",pch=19,col=Cols,lwd=2,xlab="",ylab="")
points(Res@Centers,pch=5,col=c("red","blue","green")) 

#  Compare the clustering results with the original samples 
par(mfrow=c(1,2))
plot(data[,1],data[,2],type="p",pch=19,col=rep(c("sky blue","orange","purple"),rep(10,3)),
     lwd=2,xlab="",ylab="",main="Original Data")
plot(data[,1],data[,2],type="p",pch=19,col=Cols,lwd=2,xlab="",ylab="",
     main="MKMeans Clustering Results")
points(Res@Centers,pch=5,col=c("red","blue","green")) 

# conduct MKMeans analysis with K=4 and randomly picking four samples as initial cluster centers
Res<-MKMeans(data,4,data[sample(1:30,4),],iteration=1000,tol=.95,type=1)
names(Res@Classes[[1]])<-rep("red",length(Res@Classes[[1]]))
names(Res@Classes[[2]])<-rep("blue",length(Res@Classes[[2]]))
names(Res@Classes[[3]])<-rep("green",length(Res@Classes[[3]]))
names(Res@Classes[[4]])<-rep("black",length(Res@Classes[[4]]))
Cols<-names(sort(c(Res@Classes[[1]],Res@Classes[[2]],Res@Classes[[3]],Res@Classes[[4]])))
plot(data[,1],data[,2],type="p",pch=19,col=Cols,lwd=2,xlab="",ylab="")
points(Res@Centers,pch=5,col=c("red","blue","green","black"))

#  Compare the clustering results with the original data
par(mfrow=c(1,2))
plot(data[,1],data[,2],type="p",pch=19,col=rep(c("sky blue","orange","purple"),rep(10,3)),
     lwd=2,xlab="",ylab="",main="Original Data")
plot(data[,1],data[,2],type="p",pch=19,col=Cols,lwd=2,xlab="",ylab="",
     main="MKMeans Clustering Results")
points(Res@Centers,pch=5,col=c("red","blue","green","black")) 


}
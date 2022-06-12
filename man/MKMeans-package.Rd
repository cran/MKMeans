\name{MKMeans-package}
\alias{MKMeans-package}
\docType{package}
\title{ Modern K-Means (MKMeans) Clustering.}
\description{ It's a Modern K-Means clustering algorithm allowing data of any number of dimensions, any initial center, and any number of clusters to expect.

}  

\details{
\tabular{ll}{
Package: \tab MKMeans\cr
Type: \tab Package\cr
Version: \tab 2.1\cr
Date: \tab 2022-06-12\cr
License: \tab GPL-2\cr
}
 }
\author{ Yi Ya, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang  }
\references{ Yarong Yang(Yi Ya) and Jacob Zhang.(2022)  MKMeans: A Modern K-Means Clustering Algorithm. submitted to Journal of Statistical Association
}

\keyword{package }

\examples{
x<-rnorm(20,0,1)
y<-rnorm(20,1,1)
data.test<-cbind(x,y)
Res<-MKMeans(data.test,3,1,iteration=1000,tol=.9,type=1)
Ress<-Res
names(Ress@Classes[[1]])<-rep("red",length(Res@Classes[[1]]))
names(Ress@Classes[[2]])<-rep("blue",length(Res@Classes[[2]]))
names(Ress@Classes[[3]])<-rep("green",length(Res@Classes[[3]]))
Cols<-names(sort(c(Ress@Classes[[1]],Ress@Classes[[2]],Ress@Classes[[3]])))
plot(x,y,type="p",col=Cols,lwd=2)
points(Res@Centers,pch=15,col=c("red","blue","green")) 
}


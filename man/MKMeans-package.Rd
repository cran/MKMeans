\name{MKMeans-package}
\alias{MKMeans-package}
\docType{package}
\title{ Modern K-Means (MKMeans) Clustering.}
\description{ It's a modern K-Means clustering method with no limitation on the selection of initial centers but only the number of clusters is needed.
}  

\details{
\tabular{ll}{
Package: \tab MKMeans\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2021-11-06\cr
License: \tab GPL-2\cr
}
 }
\author{ Yi Ya, Yarong Yang, and Jacob Zhang  }
\references{ Yi Ya, Yarong Yang, and Jacob Zhang.  MKMeans: A Modern K-Means Clustering Algorithm. ongoing
}

\keyword{package }

\examples{
x<-c(-0.2373498, -1.2630631,  1.6861395, -0.8956045,  2.2241156, -0.5894360,
-1.2108904,  1.1840438, -1.6438053 , 1.7992582)
y<-c(1.7756941,  0.7164913,  0.7611635, -0.3304464,  0.7848649,  2.2317787,
0.9815304,  0.1834405,  0.2749449,  0.9247629)
data.test<-cbind(x,y)
Res<-MKMeans(data.test,2,1,iteration=1000,tol=.001,type=1)
Ress<-Res
names(Ress@Classes[[1]])<-rep("red",length(Res@Classes[[1]]))
names(Ress@Classes[[2]])<-rep("blue",length(Res@Classes[[2]]))
Cols<-names(sort(c(Ress@Classes[[1]],Ress@Classes[[2]])))
plot(x,y,type="p",col=Cols,lwd=2)
points(Res@Centers,pch=15,col=c("red","blue")) 
}


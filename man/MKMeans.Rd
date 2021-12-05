\name{MKMeans}
\alias{MKMeans}
\title{Modern K-Means clustering.}
\description{
    It's a Modern K-Means clustering algorithm allowing data of any number of dimensions, any initial center, and any number of clusters to expect.
}  
\usage{
MKMeans(data, K, initial, iteration, tol, type)
}
\arguments{
  \item{data}{ Numeric. An observation matrix with each row being an oberservation.}
  \item{K}{ Integer.  The number of clusters expected.}
  \item{initial}{ Numeric. Either the selected initial center matrix with each row being an observation, or 1 for the first K rows of the data matrix being the intial center.     }
  \item{iteration}{ Integer. The number of the most iterations wanted for the clustering process. }
  \item{tol}{ Numeric. The minimum acceptable percentage of stable observations to stop the clustering process, basically greater than 0.5 to guarantee the value of the results.. }
  \item{type}{ Integer. The type of distance between observations. 1 for Euclidean distance. 2 for Manhattan distance. 3 for maximum deviation among dimensions.}
}

\value{An object of class MKMean.}
\author{Yi Ya and Yarong Yang }
\references{Yi Ya, Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang. MKMeans: A Modern K-Means Clustering Algorithm. ongoing
}
\examples{
x<-rnorm(20,0,1)
y<-rnorm(20,1,1)
data.test<-cbind(x,y)
Res<-MKMeans(data.test,3,1,iteration=1000,tol=.95,type=1)
Ress<-Res
names(Ress@Classes[[1]])<-rep("red",length(Res@Classes[[1]]))
names(Ress@Classes[[2]])<-rep("blue",length(Res@Classes[[2]]))
names(Ress@Classes[[3]])<-rep("green",length(Res@Classes[[3]]))
Cols<-names(sort(c(Ress@Classes[[1]],Ress@Classes[[2]],Ress@Classes[[3]])))
plot(x,y,type="p",col=Cols,lwd=2)
points(Res@Centers,pch=15,col=c("red","blue","green")) 
}


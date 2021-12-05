\name{C.f}
\alias{C.f}
\title{Finding the center of a cluster.}
\description{
    It's a function of finding the center of a cluster.
}  
\usage{
C.f(dat, type)
}
\arguments{
  \item{dat}{ Numeric. A cluster matrix with each row being an observaion.}
   \item{type}{ Integer. The type of distance between observations. 1 for Euclidean distance. 2 for Manhattan distance. 3 for maximum deviation along dimensions.}
}

\value{A vector.}
\author{Yi Ya and Yarong Yang }
\references{Yi Ya, Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang. MKMeans: A Modern K-Means Clustering Algorithm. ongoing
}
\examples{
x<-rnorm(5,0,1)
y<-rnorm(5,1,1)
data<-cbind(x,y)
Res<-C.f(dat=data,type=1)
}


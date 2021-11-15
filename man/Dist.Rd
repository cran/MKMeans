\name{Dist}
\alias{Dist}
\title{ Finding the distance between two observations.}
\description{
    It's a function of finding the distance between two observations.
}  
\usage{
Dist(x,y,type)
}
\arguments{
  \item{x}{ Numeric. A vector denoting an observation.}
  \item{y}{ Numeric. A vector denoting an observation.}
  \item{type}{ Integer. The type of distance between observations. 1 for Euclidean distance.}
}
\value{A list.}
\author{Yi Ya and Yarong Yang }
\references{Yi Ya, Yarong Yang, and Jacob Zhang. MKMeans: A Modern K-Means Clustering Algorithm. ongoing
}
\examples{
x<-rnorm(10,0,1)
y<-rnorm(10,1,1)
z<-rnorm(10,2,1)
data<-cbind(x,y,z)
Res<-Dist(data[1,],data[2,],type=1)
}


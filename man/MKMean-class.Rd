\name{MKMean}
\Rdversion{3.1}
\docType{class}
\alias{MKMean-class}

\title{ Class to contain the results from function MKMeans. }

\description{
The function MKMeans return object of class MKMean that contains the number of clusters, 
the center of each cluster, and the observations in each cluster.
}

\section{Objects from the Class}{
new("MKMean",K=new("numeric"),Centers=new("matrix"),Classes=new("list"),Clusters=new("list"))
}
\section{Slots}{
  \describe{
    \item{\code{K}:}{An integer being the number of clusters.}
    \item{\code{Centers}:}{A numeric matrix with each row being center of a cluster.}
    \item{\code{Classes}:}{An integer list showing the original indexes of the observations in each cluster.}
    \item{\code{Clusters}:}{ A numeric list showing the observations in each cluster.  }
    }
}

\references{Yarong Yang and Jacob Zhang.(2024) MKMeans: A Modern K-Means Clustering Algorithm. 
}
\author{
Yarong Yang
}
\examples{
showClass("MKMean")
}
\keyword{classes}

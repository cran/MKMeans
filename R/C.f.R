C.f<-function(dat,type) {        
	  sq<-vector()
	  for(i in 1:nrow(dat)) {
		    D<-vector()
		    for(j in 1:nrow(dat))
		        D[j]<-Dist(dat[i,],dat[j,],type)$d
		    sq[i]<-sum(D^2)
	  }
	  g<-which(sq==min(sq))[1]
	  dat[g,]
}



C.f<-function(dat,type) {    
   
	  sq<-vector()
        if(nrow(dat)==1) { 
            ct<-dat 
        } else {
	      for(i in 1:nrow(dat)) {
		    D<-vector()
		    for(j in 1:nrow(dat))
		        D[j]<-Dist(dat[i,],dat[j,],type)
		    sq[i]<-sqrt(sum(D^2))
	      }
	      g<-which(sq==min(sq))[1]
	      ct<-dat[g,]
       }
       
       ct
}



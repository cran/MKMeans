MKMeans<-function(data, K, initial, iteration, tol, type) {
	
	  if(initial==1) C<-data[1:K,] else C<-initial
	  cl<-list()
	  CL<-list()
	  cl.old<-list()
	  it<-0
	  p1<-p2<-p<-0
	  for(j in 1:K)  cl.old[[j]]<-vector()
	
	  while(it<iteration&p<tol) {	  	
		     it<-it+1
	      for(j in 1:K) cl[[j]]<-vector()    	  
	      for(i in 1:nrow(data)) {
	          D<-vector()
		        for(j in 1:K) 
			           D[j]<-Dist(data[i,],C[j,],type)$d
		        t<-which(D==min(D))
		        l<-cl[[t]]
		        cl[[t]]<-c(l,i)		
	      }
	
	      for(j in 1:K) {
		        CL[[j]]<-data[cl[[j]],]
		        dat<-CL[[j]]
           C[j,]<-C.f(dat,type)		
	      }	
	      
	      mat<-0
	      for(j in 1:K) 
	         mat<-mat+length(intersect(cl.old[[j]],cl[[j]]))
	      
	      p1<-p2
	      p2<-mat/nrow(data)
	      p<-min(p1,p2)
	      cl.old<-cl
	      cat("Iteration:",it,"-----","Matching1:",p1,"-----","Matching2:",p2,"\n")
	   }
	
    	return(new("MKMean",K=K,Centers=C,Classes=cl,Clusters=CL))
}






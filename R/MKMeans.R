MKMeans<-function(data, K, initial, iteration, tol, type) {
	
	  if(initial==1) {
            C<-data[1:K,]
        } else { C<-initial}

	  cl<-list()         # index cluster
	  CL<-list()         # obs cluster
        cl.previous<-list()
        for(i in 1:15) cl.previous[[i]]<-list()
        cl.old<-list()
	  it<-0
        p<-0

	  for(j in 1:K)  {
            cl.old[[j]]<-vector()
            for(i in 1:15) cl.previous[[i]][[j]]<-vector()
        }                

	  while(it<iteration&p<tol) {	  	
		it<-it+1
	      for(j in 1:K) cl[[j]]<-vector()    	  
	      for(i in 1:nrow(data)) {
	          D<-vector()
		        for(j in 1:K) 
			           D[j]<-Dist(data[i,],C[j,],type)
		        t<-which(D==min(D))[1]
		        l<-cl[[t]]
		        cl[[t]]<-c(l,i)		
	      }

            S<-0           #the sum of distances (meaning of MSE)
	      for(j in 1:K) {
		        CL[[j]]<-data[cl[[j]],]
		        dat<-CL[[j]]
                    if(length(cl[[j]])==1) dat<-matrix(CL[[j]],nrow=1)    
                    C[j,]<-C.f(dat,type)	
                    for(s in cl[[j]]) 
                             S<-S+Dist(data[s,],C[j,],type)	
	      }	


	      
	      mat<-0          
	      for(j in 1:K) {
               mat1<-cl.previous[[15]][[j]]
               for(i in 1:14) {                   
                    mat1<-intersect(mat1,cl.previous[[15-i]][[j]])
               }
               mat<-mat+length(intersect(intersect(mat1,cl.old[[j]]),cl[[j]]))
            }
            
	      p<-mat/nrow(data)

            for(i in 1:14) cl.previous[[i]]<-cl.previous[[i+1]]
            cl.previous[[15]]<-cl.old
            cl.old<-cl

            cat("Iteration:",it,"-----","Stable Percentage:",p,"-----","Total Sum of Distances: ",S,"\n")
	   }


	
    	return(new("MKMean",K=K,Centers=C,Classes=cl,Clusters=CL))
}





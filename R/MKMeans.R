MKMeans<-function(data,K,initial,iteration=1000,type){

        if(is.matrix(initial)) {
           if(nrow(initial)==K) C<-initial else {
                cat("The number of initial centers doesn't match the number of clusters. The first K observations of the data set are automatically selected as initial cluster centers.\n")
                C<-data[1:K,]
           }
        } else C<-data[1:K,]
   
        Vec.comp<-function(L1,L2){
           t<-rep(0,length(L1))
           for(i in 1:length(L1)) {
               if(L1[i]!=L2[i]) t[i]<-1
           }
           if(sum(t)==0) return(TRUE) else return(FALSE)
        }

        M.comp<-function(M1,M2) {
           for(i in 1:nrow(M1)) {
              V<-vector()
              for(j in 1:nrow(M2))
                 V[j]<-Vec.comp(M1[i,],M2[j,])
              if(all(!V)) return(FALSE)
           }
           return(TRUE)
        }

        C.new<-matrix(0,nrow=K,ncol=ncol(data))
        M.temp<-matrix(0,nrow=nrow(data),ncol=K+1)
        for(i in 1:nrow(data)) {
            for(j in 1:K) 
                M.temp[i,j]<-Dist(data[i,],C[j,],type)
            M.temp[i,K+1]<-which(M.temp[i,1:K]==min(M.temp[i,1:K]))
        }
        
        for(i in 1:K) 
            C.new[i,]<-C.f(data[which(M.temp[,K+1]==i),],type)
        p<-1
        cat("Iteration: ",p,"\n")
        
        while(!M.comp(C.new,C)&p<iteration) {
            C<-C.new
            M.temp<-matrix(0,nrow=nrow(data),ncol=K+1)
            for(i in 1:nrow(data)) {
                for(j in 1:K) 
                    M.temp[i,j]<-Dist(data[i,],C[j,],type)
                M.temp[i,K+1]<-which(M.temp[i,1:K]==min(M.temp[i,1:K]))
            }
            for(i in 1:K) 
                C.new[i,]<-C.f(data[which(M.temp[,K+1]==i),],type)
            p<-p+1
            cat("Iteration: ",p,"\n")
        }

        cl<-list()
        CL<-list()
        for(i in 1:K) {
            ind<-which(M.temp[,K+1]==i)
            cl[[i]]<-ind
            CL[[i]]<-data[ind,]
        }
        
        SS<-NULL
        for(i in 1:K) {
            S<-0
            for(j in 1:nrow(CL[[i]]))
                S<-S+Dist(C.new[i,],CL[[i]][j,],type)
            SS<-c(SS,S)
        }
        SS<-c(SS,sum(SS))
        cat("SSE = ",SS,"\n")       
        
    	  return(new("MKMean",K=K,Centers=C.new,Classes=cl,Clusters=CL,SSE=SS))
}


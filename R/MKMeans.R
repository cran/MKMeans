MKMeans<-function(data,K,method,initial,iteration=1000,type){
   
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
      
        InitialDeterm<-function(data,C.temp,K,type) {
            C.L<-NULL
            for(i in 1:nrow(C.temp)) {
                 observation<-C.temp[i,]
                 if(length(observation)!=ncol(data)) break
            
                 L<-vector()
                 for(i in 1:nrow(data))
                     L[i]<-Dist(data[i,],observation,type)  
                 ind<-sort(L,index=TRUE)$ix[1:(K+1)]
                 for(j in 1:length(ind)) {
                     if(!is.element(ind[j],C.L)) {
                         C.L<-c(C.L,ind[j]) 
                         break
                     }
                 }      
            }
            
            return(data[C.L,])
        }
        
        if(method=="YY") {       

            Q<-NULL
            for(i in 1:ncol(data)) {
                d<-(max(data[,i])-min(data[,i]))/(K+1)
                m<-vector()
                for(j in 1:K)
                   m[j]<-min(data[,i])+d*j
                Q<-cbind(Q,m)
            }
            colnames(Q)<-1:ncol(data)
            IND<-permutations(K,ncol(data),1:K,repeats.allowed=TRUE)  
            InitialCenters<-matrix(0,ncol=ncol(IND),nrow=nrow(IND))  
            for(i in 1:nrow(IND)) 
                for(j in 1:ncol(IND))
                    InitialCenters[i,j]<-Q[,j][IND[i,j]]

            M<-combn(1:nrow(InitialCenters),K)
            M.SS<-NULL
            for(k in 1:ncol(M)){
                ind<-M[,k]
                C.temp<-InitialCenters[ind,]
                C<-InitialDeterm(data,C.temp,K,type)
                C.new<-matrix(0,nrow=K,ncol=ncol(data))
                M.temp<-matrix(0,nrow=nrow(data),ncol=K+1)
                for(i in 1:nrow(data)) {
                    for(j in 1:K)
                        M.temp[i,j]<-Dist(data[i,],C[j,],type)
                    M.temp[i,K+1]<-which(M.temp[i,1:K]==min(M.temp[i,1:K]))[1]
                }
            
                for(i in 1:K) {
                    index<-which(M.temp[,K+1]==i)
                    if(length(index)==1) {
                        dat<-t(as.matrix(data[which(M.temp[,K+1]==i),]))
                    } else {
                        dat<-data[which(M.temp[,K+1]==i),]
                    }
                    C.new[i,]<-C.f(dat,type)
                }
                p<-1

                while(!M.comp(C.new,C)&p<iteration) {
                    C<-C.new
                    M.temp<-matrix(0,nrow=nrow(data),ncol=K+1)
                    for(i in 1:nrow(data)) {
                        for(j in 1:K)
                            M.temp[i,j]<-Dist(data[i,],C[j,],type)
                        M.temp[i,K+1]<-which(M.temp[i,1:K]==min(M.temp[i,1:K]))                    
                    }
                    for(i in 1:K) {
                        index<-which(M.temp[,K+1]==i)
                        if(length(index)==1) {
                            dat<-t(as.matrix(data[which(M.temp[,K+1]==i),]))
                        } else {
                            dat<-data[which(M.temp[,K+1]==i),]
                        }
                        C.new[i,]<-C.f(dat,type)
                    }
                    p<-p+1
                }
            
                cl<-list()
                CL<-list()
                for(i in 1:K) {
                    ind<-which(M.temp[,K+1]==i)
                    cl[[i]]<-ind
                    if(length(ind)==1) CL[[i]]<-t(as.matrix(data[ind,])) else CL[[i]]<-data[ind,]
                } 

                SS<-NULL
                for(i in 1:K) {
                    S<-0
                    for(j in 1:nrow(CL[[i]]))
                       S<-S+Dist(C.new[i,],CL[[i]][j,],type)
                    SS<-c(SS,S)
                }
                SS<-c(SS,sum(SS))
                M.SS<-cbind(M.SS,SS)
            } 
  
            TSS<-M.SS[nrow(M.SS),]   
            id<-which(TSS==min(TSS))[1]  

            ind<-M[,id]
            C.temp<-InitialCenters[ind,]
            C<-InitialDeterm(data,C.temp,K,type)

        } 
        if(method=="initial"){
            if(is.null(initial)) 
                 return("The initial cluster centers are missing!")
            if(is.matrix(initial)) {
                 if(nrow(initial)==K) C<-initial else {
                      cat("The number of initial cluster centers doesn't match the number of clusters. The first K observations of the data set are automatically selected as initial cluster centers.\n")
                      C<-data[1:K,]
                 }
            } else C<-data[1:K,]
        }

        C.new<-matrix(0,nrow=K,ncol=ncol(data))
        M.temp<-matrix(0,nrow=nrow(data),ncol=K+1)
        for(i in 1:nrow(data)) {
            for(j in 1:K)
                M.temp[i,j]<-Dist(data[i,],C[j,],type)
            M.temp[i,K+1]<-which(M.temp[i,1:K]==min(M.temp[i,1:K]))[1]
        }
            
        for(i in 1:K) {
            index<-which(M.temp[,K+1]==i)
            if(length(index)==1) {
                dat<-t(as.matrix(data[which(M.temp[,K+1]==i),]))
            } else {
                dat<-data[which(M.temp[,K+1]==i),]
            }
            C.new[i,]<-C.f(dat,type)
        }
        p<-1

        while(!M.comp(C.new,C)&p<iteration) {
              C<-C.new
              M.temp<-matrix(0,nrow=nrow(data),ncol=K+1)
              for(i in 1:nrow(data)) {
                  for(j in 1:K)
                      M.temp[i,j]<-Dist(data[i,],C[j,],type)
                  M.temp[i,K+1]<-which(M.temp[i,1:K]==min(M.temp[i,1:K]))                    
              }
              for(i in 1:K) {
                  index<-which(M.temp[,K+1]==i)
                  if(length(index)==1) {
                     dat<-t(as.matrix(data[which(M.temp[,K+1]==i),]))
                  } else {
                     dat<-data[which(M.temp[,K+1]==i),]
                  }
                  C.new[i,]<-C.f(dat,type)
              }
              p<-p+1
        }
            
        cl<-list()
        CL<-list()
        for(i in 1:K) {
            ind<-which(M.temp[,K+1]==i)
            cl[[i]]<-ind
            if(length(ind)==1) CL[[i]]<-t(as.matrix(data[ind,])) else CL[[i]]<-data[ind,]
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





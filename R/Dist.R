Dist<-function(x,y,type) { 

	  diff<-x-y       
	  if(type==1) d<-sqrt(sum(diff^2))
        if(type==2) d<-sum(abs(diff))
        if(type==3) d<-max(abs(diff))

        d
}



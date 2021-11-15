Dist<-function(x,y,type) {        
	  if(type==1) {
	      d<-sqrt(sum((x-y)^2))
	      diff<-x-y
	  } else {
		print("Euclidean Distance are required!")
	  }
	  return(list(d=d,diff=diff))
}



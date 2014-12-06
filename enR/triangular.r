#generacion de conjunto de
#variables triangulares con los mismos parámetros(n elementos)

triangular <- function(a,b,c,n){
t <- matrix(0, nrow = 1, ncol = n)
for (i in 1:n){
	u<-runif(1)
	w<-(b-a)/(c-a)
	if (u<=w){
		t[1,i]<-a+sqrt((c-a)*(b-a)*u)
	}else{
		t[1,i]<-c-sqrt((c-a)*(c-b)*(1-u))
	}
}
return(t)
}


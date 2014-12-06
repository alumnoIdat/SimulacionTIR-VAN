#generacion de conjunto de
#variables triangulares con los mismos parámetros(n elementos)

triangpar <- function(a,b,c,n){
t <- matrix(0, nrow = 1, ncol = n)
w <- (b-a)/(c-a);
for (i in seq(1, 2*floor(10/2), 2)){
	u<-runif(1)
    if (u<=w){
    	t[1,i] <- a+sqrt((c-a)*(b-a)*u)
        t[1,i+1] <- a+sqrt((c-a)*(b-a)*(1-u))
    }
    else{
    	t[1,i] <- c-sqrt((c-a)*(c-b)*(1-u));
        t[1,i+1] <- c-sqrt((c-a)*(c-b)*u);
    }
}
   
if (n%%2 == 1){
    u<-runif(1)
    if (u<=w){
    	t[1,n] <- a+sqrt((c-a)*(b-a)*u);    
    }   
    else{
    	t[1,n] <- c-sqrt((c-a)*(c-b)*(1-u));
    }
}

return(t)
}


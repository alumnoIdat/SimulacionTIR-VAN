tir2 <- function(inv_ini,fc){
#Este es un programa alternativo para el calculo del TIR de un conjunto 
#de valores de un flujo de caja.
#inv_ini: inversion inicial
#fc: vector que contiene el flujo de caja del proyecto
aux <- print(-inv_ini, digits = (floor(log(abs(inv_ini),10)+1) + 2))
s <- c(aux);
for(i in 1:length(fc)){
    a <- print(abs(fc(i)), digits = (floor(log(abs(inv_ini),10)+1) + 2))
    b <- i
    if (fc[i]>0){
        s=[s,'+',a,'/(1+t)^',b];
    }else{
        s=[s,'-',a,'/(1+t)^',b];
    }
}
p=solve(s,'t');

j <- 1
l <- TRUE

while (l){
    if (is.numeric(p[j])){
        k <- p[j]
        l <- FALSE
    }
    j <- j+1
}
for (i in j:length(p)){
    if (is.numeric(p[i]) && p[i])>k){
        k <- p[i]
    }
}
r <- k;
return(r)
}

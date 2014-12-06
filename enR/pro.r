#Programa para la generacion de un numero no mayor de 800 iteraciones del VAN.

rm(list=ls())
system('clear')
#cat("\014") # cls

source("triangular.r")
source("triangpar.r")
source("van.r")

tddes <- as.numeric(readline("Ingrese la tasa de descuento : "))
n <- as.numeric(readline("Ingrese el número de iteraciones : "))

prod <- 1
prob <- 0
vans <- matrix(0, nrow = 1, ncol = n)
tainf <- matrix(0, nrow = 1, ncol = 5)
frec <- matrix(0, nrow = 1, ncol = 20)
freca <- matrix(0, nrow = 1, ncol = 20)
for (i in 1:n){
    af <- triangular(60,70,100,1) #Activo fijo
    ac <- triangular(25,30,40,1)  #Activo circulante
    tainf[1] <- triangular(12,15,18,1) #tainf: tasa actual de inflacion 
    tainf[2] <- triangular(12,15,18,1)
    tainf[3] <- triangular(15,18,22,1)
    tainf[4] <- triangular(18,20,25,1)
    tainf[5] <- triangular(19,22,28,1)
    tainf <- tainf/100;
    fantesimp <- triangular(30,40,45,5)
        for (k in 1:5){
       		prod <- prod*(1+tainf[k])
        }
        st <- NULL
        for (j in 1:5){
        	st[j] <- fantesimp[j]*0.5+(0.2*af*0.5-ac*tainf[j]*prod/(1+tainf[1]))/prod
        }
    vr <- ac+0.2*af*0.5
    st[5] <- st[5]+vr
    inv <- af+ac
    vans[1,i] <- van(inv,st,tddes)
}

x <- seq(min(vans), max(vans), length.out = 21)

for (u in 1:n){
    xx <- vans[1,u]
    for (k in 1:20){
	    if(xx<=x[k+1] & xx>x[k]){
	    	frec[1,k] <- frec[1,k]+1;
	    }
    }
}

frec[1,1] <- frec[1,1]+1
freca[1,1] <- frec[1,1]

for (y in 1:19){
    freca[1,y+1] <- freca[1,y]+frec[1,y+1]
}
freca <- freca/n;

zeta <- NULL
for (i in 1:20){
    zeta[i] <- (x[i]+x[i+1])/2;
}

plot(zeta,freca);
#figure(2);
plot(zeta,frec);
media <- mean(vans[1, ]);
desviacion <- sqrt(var(vans[1, ])/n);
system('clear') #clc;

print('RESULTADOS OBTENIDOS DEL VALOR ACTUAL NETO')
print('------------------------------------------')
print('MEDIA ')
print(media)
print('DESVIACION DE LA MEDIA')
print(desviacion)
print('INTERVALO DE CONFIANZA')
print(media-1.96*desviacion)
print(media+1.96*desviacion)

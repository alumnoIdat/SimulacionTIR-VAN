function [A,B]=evaluaproyect_1 <- function(d)
#Este programa utiliza la varianza de la probabilidad de que el proyecto 
#sea rentable en el criterio de parada.
#A: Valor Actual Neto.
#B: Tasa Interna de Retorno.
#d: desviación límite.
#per: numero de periodos.
#tm: tamaño de mercado.
#cm: crecimiento de mercado.
#pm: participacion de mercado.
#co: costos operativos.
#ti: tasa impositiva.
#re: reinversion.
#clp: crecimiento a largo plazo.
#td: tasa de descuento.
#inv: inversion inicial.

cont=0; van=NULL; tasa=NULL; cm=NULL; pm=NULL;
co=NULL; re=NULL; clp=NULL; td=NULL;
per <- 6
sk <- Inf
iter <- 1

while (sk>d){                     
tm <- 1030000                             #########
cm[iter] <- (rnorm()*0.5/100)+4/100;                   #
pm[iter] <- (rnorm()*0.5/100)+5/100;                   #
co[iter] <- 0.7+(0.8-0.7)*runif();                      #   Asignacion de
ti=0.4;                                           ### variables aleatorias.
re[iter] <- (rnorm()*1/100)+0.1;                       #
clp[iter] <- 2.5/100+(3.5/100-2.5/100)*runif();         #   
td[iter] <- (11/100+(13/100-11/100)*runif());           #
inv=24000;                                #########

# tm=1030000;                         #########
# cm=4/100;                                   #
# pm=5/100;                                   #
# co=75/100;                                  #   Asignacion de
# ti=40/100;                                  ### variables fijas
# re=10/100;                                  #
# clp=3/100;                                  #   
# td=12/100;                                  #
# inv=24000;                           ########


flujo <- matrix(0, nrow = 9, ncol = (per+1))
flujo[1,1] <- tm*pm[iter];

for (j in 2:per){
    flujo[1,j] <- flujo[1,1]*(1+cm[iter])^(j-1);       #Cálculo de las ventas
}

flujo[2,1:per] <- flujo[1,1:per]*co[iter];             #Costos  
flujo[3,1:per] <- flujo[1,1:per]-flujo[2,1:per];       # Utilidad antes de intereses e Impuestos
flujo[4,1:per] <- flujo[3,1:per]*ti;                   # Impuestos  
flujo[5,1:per] <- flujo[3,1:per]-flujo[4,1:per];       # Utilidad despues de impuestos 
flujo[6,1:per] <- flujo[1,1:per]*re[iter];             # Fondos de reinversion.  
flujo[7,1:per] <- flujo[5,1:per]-flujo[6,1:per];       # Flujo de fondos Libre
flujo[7,per+1] <- flujo[7,per]*(1+clp[iter])/(td[iter]-clp(iter)); #Residual del Flujo de fondos

for (j in 1:per){                                  ########
    flujo[8,j] <- 1/(1+td[iter])^(j);                  ########%Factor de descuento
}

flujo[9,1:per] <- flujo[7,1:per].*flujo[8,1:per];      #Valor Presente
flujo[9,per+1] <- flujo[9,per]*(1+clp[iter])/(td[iter]-clp[iter]); %Residual del Valor Presente

van[iter] <- sum(flujo[9,:])-inv;                     #Valor actual neto.
tasa[iter]=tir2(inv,flujo[7,:]);              #Tasa interna de retorno  

k=0;m=0;
for (i in 1:iter){
    if(van[i]>0){
        k <- k+1;
    }
    if(tasa[i]>0){
        m <- m+1;
    }
}
p[iter] <- k/iter;
r[iter] <- m/iter;

if (iter>30){
    sk <- sqrt(p[iter]*(1-p[iter])/iter);
}
iter <- iter+1;
}

print('Numero de iteraciones:')
Iteracion <- iter-1
print(Iteracion)
print('Ultima desviación hallada:')
Desviacion <- sk
print(Desviacion)

print('Probabilidad de rentabilidad de la inversion en el proyecto usando el VAN')
print( p(iter-1))

print('Probabilidad de rentabilidad de la inversion en el proyecto usando el TIR')
print( r(iter-1))

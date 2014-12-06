#calculo del Valor Actual Neto

van<-function(inv,s,r){
#inv es la inversion
#s es el vector de flujos de caja.
#r es la tasa de descuento.
	van<- (-1)*inv
	flujos <- 0
	cant <- length(s)
	for (i in 1:cant){
	    flujos <- flujos+s[i]/(1+r)^i
	}
	van <- van + flujos
	return(van)
}

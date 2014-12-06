function van=van(inversion,s,r)%calculo del Valor Actual Neto
%inv es la inversion
%s es el vector de flujos de caja.
%r es la tasa de descuento.
van= (-1)*inversion;
flujos=0;
cant=length(s);
for i=1:cant
    flujos=flujos+s(i)/(1+r)^i;
end
van=van+flujos;

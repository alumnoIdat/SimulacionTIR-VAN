function r = tir2(inv_ini,fc)
%Este es un programa alternativo para el calculo del TIR de un conjunto 
%de valores de un flujo de caja.
%inv_ini: inversion inicial
%fc: vector que contiene el flujo de caja del proyecto
c=sprintf('%0.2f',-inv_ini);
s=[c];
for i=1:length(fc)
    a=sprintf('%0.2f',abs(fc(i)));
    b=sprintf('%d',i);
    if fc(i)>0
        s=[s,'+',a,'/(1+t)^',b];
    else
        s=[s,'-',a,'/(1+t)^',b];
    end
end
p=solve(s,'t');
j=1;l=0;
while l==0
    if isreal(p(j))==1
        k=p(j);
        l=1;
    end
    j=j+1;
end
for i=j:length(p)
    if isreal(p(i))==1 && double(p(i))>double(k)
        k=p(i);
    end
end
r=double(k);
        
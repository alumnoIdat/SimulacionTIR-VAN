function [A,B]=evaluaproyect_1(d)
%Este programa utiliza la varianza de la probabilidad de que el proyecto 
%sea rentable en el criterio de parada.
%A: Valor Actual Neto.
%B: Tasa Interna de Retorno.
%d: desviaci�n l�mite.
%per: numero de periodos.
%tm: tama�o de mercado.
%cm: crecimiento de mercado.
%pm: participacion de mercado.
%co: costos operativos.
%ti: tasa impositiva.
%re: reinversion.
%clp: crecimiento a largo plazo.
%td: tasa de descuento.
%inv: inversion inicial.

cont=0; van=[]; tasa=[]; cm=[]; pm=[];
co=[]; re=[]; clp=[]; td=[];
per=6;
sk=inf;
iter=1;

while sk>d                     
tm=1030000;                               %%%%%%%%%
cm(iter)=(randn*0.5/100)+4/100;                   %
pm(iter)=(randn*0.5/100)+5/100;                   %
co(iter)=0.7+(0.8-0.7)*rand;                      %   Asignacion de
ti=0.4;                                           %%% variables aleatorias.
re(iter)=(randn*1/100)+0.1;                       %
clp(iter)=2.5/100+(3.5/100-2.5/100)*rand;         %   
td(iter)=(11/100+(13/100-11/100)*rand);           %
inv=24000;                                %%%%%%%%%

% tm=1030000;                         %%%%%%%%%
% cm=4/100;                                   %
% pm=5/100;                                   %
% co=75/100;                                  %   Asignacion de
% ti=40/100;                                  %%% variables fijas
% re=10/100;                                  %
% clp=3/100;                                  %   
% td=12/100;                                  %
% inv=24000;                          %%%%%%%%%


flujo=zeros(9,per+1);
flujo(1,1)=tm*pm(iter);

for j=2:per,
    flujo(1,j)=flujo(1,1)*(1+cm(iter))^(j-1);       %C�lculo de las ventas
end

flujo(2,1:per)=flujo(1,1:per)*co(iter);             %Costos  
flujo(3,1:per)=flujo(1,1:per)-flujo(2,1:per);       %Utilidad antes de intereses e Impuestos
flujo(4,1:per)=flujo(3,1:per)*ti;                   %Impuestos  
flujo(5,1:per)=flujo(3,1:per)-flujo(4,1:per);       %Utilidad despues de impuestos 
flujo(6,1:per)=flujo(1,1:per)*re(iter);             %Fondos de reinversion.  
flujo(7,1:per)=flujo(5,1:per)-flujo(6,1:per);       %Flujo de fondos Libre
flujo(7,per+1)=flujo(7,per)*(1+clp(iter))/(td(iter)-clp(iter)); %Residual del Flujo de fondos

for j=1:per,                                  %%%%%%%
    flujo(8,j)=1/(1+td(iter))^(j);                  %%%%Factor de descuento
end                                           %%%%%%% 

flujo(9,1:per)=flujo(7,1:per).*flujo(8,1:per);      %Valor Presente
flujo(9,per+1)=flujo(9,per)*(1+clp(iter))/(td(iter)-clp(iter)); %Residual del Valor Presente

van(iter)= sum(flujo(9,:))-inv;                     %Valor actual neto.
tasa(iter)=tir2(inv,flujo(7,:));              %Tasa interna de retorno  

k=0;m=0;
for i=1:iter
    if(van(i)>0)
        k=k+1;
    end
    if(tasa(i)>0)
        m=m+1;
    end
end
p(iter)=k/iter;
r(iter)=m/iter;

if iter>30
    sk=sqrt(p(iter)*(1-p(iter))/iter);
end
iter=iter+1;
end

fprintf('Numero de iteraciones:');
Iteracion=iter-1
fprintf('Ultima desviaci�n hallada:');
Desviacion=sk

fprintf('Probabilidad de rentabilidad de la inversion en el proyecto usando el VAN');
p(iter-1)

fprintf('Probabilidad de rentabilidad de la inversion en el proyecto usando el TIR');
r(iter-1)
function [A,B]=evaluaproyect_2(d)
%Este programa utiliza la varianza del valor actual neto en el criterio 
%de parada sin hacer uso del metodo de reduccion de varianza.
%A: Valor Actual Neto.
%B: Tasa Interna de Retorno.
%d: desviación límite.
%per: numero de periodos.
%tm: tamaño de mercado.
%cm: crecimiento de mercado.
%pm: participacion de mercado.
%co: costos operativos.
%ti: tasa impositiva.
%re: reinversion.
%clp: crecimiento a largo plazo.
%td: tasa de descuento.
%inv: inversion inicial.

van=[]; tasa=[]; cm=[]; pm=[];
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
    flujo(1,j)=flujo(1,1)*(1+cm(iter))^(j-1);       %Cálculo de las ventas
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

if iter==1
   m_van(iter)=van(iter);
   m_tir(iter)=tasa(iter);
   var_van(iter)=0;
else
    m_van(iter)=m_van(iter-1)+(van(iter)-m_van(iter-1))/iter;
    m_tir(iter)=m_tir(iter-1)+(tasa(iter)-m_tir(iter-1))/iter;
    var_van(iter)=(1-1/(iter-1))*var_van(iter-1)+iter*(m_van(iter)-m_van(iter-1))^2;
end

if iter>30
    sk=sqrt(var_van(iter)/iter);
end
    iter=iter+1;
end

fprintf('Numero de iteraciones:');
Iteracion=iter-1
fprintf('Ultima desviación hallada:');
Desviacion=sk

k=0;m=0;
for i=1:iter-1
    if(van(i)>0)
        k=k+1;
    end
    if(tasa(i)>0)
        m=m+1;
    end
end

fprintf('Probabilidad de rentabilidad de la inversion en el proyecto usando el VAN');
Probabilidad=k/(iter-1)
fprintf('Probabilidad de rentabilidad de la inversion en el proyecto usando el TIR');
Probabilidad=m/(iter-1)

fprintf('Valor Actual Neto esperado:');
m_van(iter-1)
fprintf('Tasa Interna de Retorno esperado:');
m_tir(iter-1)


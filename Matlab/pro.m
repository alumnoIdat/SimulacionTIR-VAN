%Programa para la generacion de un numero no mayor de 800 iteraciones del VAN.
clear;
clc;
tddes=input('Ingrese la tasa de descuento : ');
n=input('Ingrese el número de iteraciones : ');
prod=1;
prob=0;
vans=zeros(1,n);
tainf=zeros(1,5);
frec=zeros(1,20);
freca=zeros(1,20);
for i=1:n,
    af=triangular(60,70,100,1); %Activo fijo
    ac=triangular(25,30,40,1);  %Activo circulante
    tainf(1)=triangular(12,15,18,1); %tainf: tasa actual de inflacion 
    tainf(2)=triangular(12,15,18,1);
    tainf(3)=triangular(15,18,22,1);
    tainf(4)=triangular(18,20,25,1);
    tainf(5)=triangular(19,22,28,1);
    tainf=tainf/100;
    fantesimp=triangular(30,40,45,5);
        for k=1:5,
            prod=prod*(1+tainf(k));
        end
        for j=1:5,
            st(j)=fantesimp(j)*0.5+(0.2*af*0.5-ac*tainf(j)*prod/(1+tainf(1)))/prod;
        end
    vr=ac+0.2*af*0.5;
    st(5)=st(5)+vr;
    inv=af+ac;
    vans(1,i)=van(inv,st,tddes);
end
x=linspace(min(vans),max(vans),21);
for u=1:n,
    xx=vans(1,u);
    for k=1:20,
        if(xx<=x(k+1) && xx>x(k)),
            frec(1,k)=frec(1,k)+1;
        end
    end
end
frec(1,1)=frec(1,1)+1;
freca(1,1)=frec(1,1);
for y=1:19
    freca(1,y+1)=freca(1,y)+frec(1,y+1);
end
freca=freca/n;
for i=1:20,
    zeta(i)=(x(i)+x(i+1))/2;
end
plot(zeta,freca);
figure(2);
plot(zeta,frec);
media=mean(vans);
desviacion=sqrt(var(vans)/n);
clc;
disp('RESULTADOS OBTENIDOS DEL VALOR ACTUAL NETO');
disp('------------------------------------------');
disp('MEDIA ');
disp(media);
disp('DESVIACION DE LA MEDIA');
disp(desviacion);
disp('INTERVALO DE CONFIANZA');
disp(media-1.96*desviacion);
disp(media+1.96*desviacion);

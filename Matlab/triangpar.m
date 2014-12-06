function t=triangpar(a,b,c,n)%generacion de conjunto de
%variables triangulares con los mismos parámetros(n elementos)
t=zeros(1,n);
w=(b-a)/(c-a);
for i=1:2:2*floor(n/2),
    u=rand;
    if u<=w,
        t(1,i)=a+sqrt((c-a)*(b-a)*u);
        t(1,i+1)=a+sqrt((c-a)*(b-a)*(1-u));
    else
        t(1,i)=c-sqrt((c-a)*(c-b)*(1-u));
        t(1,i+1)=c-sqrt((c-a)*(c-b)*u);
    end
end
if mod(n,2)==1,
    u=rand;
    if u<=w,
        t(1,n)=a+sqrt((c-a)*(b-a)*u);    
    else
        t(1,n)=c-sqrt((c-a)*(c-b)*(1-u));
    end
end

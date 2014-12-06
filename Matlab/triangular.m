function t=triangular(a,b,c,n)%generacion de conjunto de
%variables triangulares con los mismos parámetros(n elementos)
t=zeros(1,n);
for i=1:n,
    u=rand;
    w=(b-a)/(c-a);
    if u<=w,
        t(1,i)=a+sqrt((c-a)*(b-a)*u);
    else
        t(1,i)=c-sqrt((c-a)*(c-b)*(1-u));
    end
end

library(MASS)
library(boot)
library(CircStats)
set.seed(12345)
a.es=c()
b.es=c()
var.es=c()
for (i in 1:1000){
  n = 100
  k = 100
  xj = rvm(n,pi/4,3)
  ej = rvm(n,0,k)
  yj = xj + ej
  #yj[]=replace(yj,yj>2*pi,2*pi)
  w=sum(cos(yj)*cos(xj)+sin(yj)*sin(xj))
  a.es[i]=(n*sum(cos(yj))-w*sum(cos(xj)))/((n^2)-(sum(cos(xj))^2))
  b.es[i]=(w-a.es[i]*sum(cos(xj)))/n
  var.es[i]= (sum(1+(a.es[i]^2)+(b.es[i]^2)+(2*a.es[i]*b.es[i]*cos(xj))-2*b.es[i]*(cos(yj)*cos(xj)+sin(yj)*sin(xj))))/n
  error = cos(yj)-a.es[i]-b.es[i]*cos(xj)
}
a=mean(a.es)-0
b=mean(b.es)-1
variance=mean(var.es)
a
b
variance

shapiro.test(error)
ks.test(error,"pnorm",mean=mean(error),sd=sd(error))
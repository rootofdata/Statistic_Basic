#예제 3.2
xi.bar<-tapply(wear,group,mean) #각각 x바의 값
summary(fit)
str(summary(fit))

cval<-qt(p= 0.05/2,df=12,lower.tail = F) #자유도 12
MSE<-summary(fit)[[1]]$'Mean Sq'[2];

r<-4

round(xi.bar[1]+c(-1,1)*cval*sqrt(MSE/r),4)
round(xi.bar[2]+c(-1,1)*cval*sqrt(MSE/r),4)
round(xi.bar[3]+c(-1,1)*cval*sqrt(MSE/r),4)
round(xi.bar[4]+c(-1,1)*cval*sqrt(MSE/r),4)

fun<-function(x,cval,MSE,r) round(x+c(-1,1)*cval*sqrt(MSE/r),4)
fun(xi.bar[2])
lapply(xi.bar,fun,cval=cval,MSE=MSE,r=r)

#예제3.3
LSD<-round(cval*sqrt(2*MSE/r),4);LSD
xi.bar
fun2<-function(ind1,ind2,xi.bar,LSD)(abs(xi.bar[ind1]-xi.bar[ind2]))>LSD
fun2(1,2,xi.bar,LSD)
fun2(1,3,xi.bar,LSD)
fun2(1,4,xi.bar,LSD)
fun2(2,3,xi.bar,LSD)
fun2(2,4,xi.bar,LSD)
fun2(3,4,xi.bar,LSD)

TukeyHSD(fit) #투키 방법 (diff만볼것)
?TukeyHSD

pairwise.t.test(wear,group,p.adj="none")
pairwise.t.test(wear,group,p.adj="bonf")#본페로니

#예제 3.4
a1<-c(62,60,63,59,61)
a2<-c(63,67,71,64,65,66)
a3<-c(68,K,66,71,67,68,68)
a4<-c(56,62,60,61,63,64)
y<-c(a1,a2,a3,a4)

paste("a",1:4,sep="")
x<-rep(paste("a",1:4,sep=""),times=c(5,6,6,6));x
cbind(y,x)
fit<-aov(y~x);fit
anova(fit)

qf(p=0.01,df1=3,df2=19,,lower.tail.FALSE)

tapply(y,x,mean)
tapply(y,x,length)
boxplot(y~x)

pairwise.t.test(y,x,p.adj="none")

pairwise.t.test(y,x,p.adj="bonf")
TukeyHSD(fit)

bartlett.test(y~x)#등분산성 가정 검정

plot(x=fit,which=1) #잔차분석

plot(fit,2) #정규성

###6.1
#자료입력
x<-c(20,22,24,26,28,30,32,34,36,38,40,42)
y<-c(8.4,9.5,11.8,10.4,13.3,14.8,13.2,14.7,16.4,16.5,18.9,18.5)

#자료적합
fit<-lm(y~x)
fit
str(fit)
#산점도 및 적합된 회귀식 확인
par(mar=c(5,5,0,1)) #x축 5만큼 y축 5만큼 띄어라
plot(x,y,pch=16,col=4)
abline(fit,col=2,lwd=2)

resid(fit)
#상관계수
cor.test(y,x)

#적합된 결과 확인
summary(fit)
#직접 적합해 보기
fun<-function(x,y){
  res<-list()
  res[[1]]<-length(x)
  res[[2]]<-mean(x)
  res[[3]]<-mean(y)
  res[[4]]<-sum((x-mean(x))^2)
  res[[5]]<-sum((x-mean(x))*(y-mean(y)))
  names(res)<-c("n","xbar","ybar","Sxx","Sxy")
  return(res)
}
tmp<-fun(x=x,y=y);tmp
b1<-tmp$Sxy/tmp$Sxx;b1
b0<-tmp$ybar-b1*tmp$xbar;b0
fit

#유용한 함수
coef(fit);coefficients(fit) #회귀계수 추정값
fitted(fit) #적합된 반응변수 값
resid(fit) #잔차
y-fitted(fit)
predict(object=fit,newdata = data.frame(x=30)) #x=30일때 y의값을 추정하는 것
abline(v=27,col="blue")
##6.2
anova(fit)
qf(p=0.01,df1=1,df2=10,lower.tail = F)
pf(q=141.13,df1=1,df2=10,lower.tail = F)

###6.3
confint(fit,level=0.95)
predict(object=fit,newdata = data.frame(x=30),interval = "confidence")

###6.4
x<-c(20,25,24,25,32,22,28,22,30,28,21,23,26,21,15)
y<-c(36,41,39,42,49,40,48,39,45,44,35,37,42,34,32)
machine<-factor(rep(1:3,each=5))
boxplot(y~machine)

fit1<-aov(y~machine)
anova(fit1)

fit2<-aov(y~x+machine)
anova(fit2)

fit3<-aov(y~x)
anova(fit3)

anova(fit2,fit3)
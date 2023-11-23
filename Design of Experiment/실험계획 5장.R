y<-c(98,99,98.6,97.6,97.7,98,98.2,97.3,96.5,97.9,96.9,96.7)
a<-factor(rep(1:3,each=4),labels = paste("a",1:3,sep = ""))
b<-factor(rep(1:4,time=3),labels = paste("b",1:4,sep = ""))
dat<-data.frame(y,a,b)
fit<-aov(y~a+b,data = dat)
anova(fit)
summary(fit)
pairwise.t.test(y,a,p.adjust.method = "none")

y<-c(-1,-5,-6,-1,-1,-8,-1,5,2,11,-7,13,1,2,-4,1,6,1,-2,-3,-3,5,-5,4,6)
r<-factor(rep(1:5,each=5),labels = paste("r",1:5,sep = ""))
c<-factor(rep(1:5,time=5),labels = paste("c",1:5,sep = ""))
trt<-factor(c(1,2,3,4,5,2,3,4,5,1,3,4,5,1,2,4,5,1,2,3,5,1,2,3,4))
dat<-data.frame(y,r,c,trt)
fit2<-aov(y~r+c+trt)
anova(fit2)

####6
x<-c(20,22,24,26,28,30,32,34,36,38,40,42)
y<-c(8.4,9.5,11.8,10.4,13.3,14.8,13.2,14.7,16.4,16.5,18.9,18.5)
fit<-lm(y~x)
anova(fit)
summary(fit)
plot(x,y)
abline(fit)
confint(fit,level=0.95)
predict(object=fit,newdata = data.frame(x=30),interval = "confidence")





#6.4
x<-c(20,25,24,25,32,22,28,22,30,28,21,23,26,21,15)
y<-c(36,41,39,42,49,40,48,39,45,44,35,37,42,34,32)
a<-factor(rep(1:3,each=5))
dat<-data.frame(x,y,a)
boxplot(y~a)

fit1<-aov(y~a)
anova(fit1)

fit2<-aov(y~x+a)
anova(fit2)

fit3<-aov(y~x)
anova(fit3)

anova(fit2,fit3)

#####################
y1<-(y-mean(y))
syy<-sum(y1^2)
x1<-(x-mean(x))
sxx<-sum(x1^2)
sxy<-sum(x1*y1)
tyy<-((mean(y[1:5])-mean(y))^2+(mean(y[6:10])-mean(y))^2+(mean(y[11:15])-mean(y))^2)*5
txx<-((mean(x[1:5])-mean(x))^2+(mean(x[6:10])-mean(x))^2+(mean(x[11:15])-mean(x))^2)*5
txy<-((mean(y[1:5])-mean(y))*(mean(x[1:5])-mean(x))+(mean(y[6:10])-mean(y))*(mean(x[6:10])-mean(x))+(mean(y[11:15])-mean(y))*(mean(x[11:15])-mean(x)))*5
eyy<-syy-tyy
exx<-sxx-txx
exy<-sxy-txy
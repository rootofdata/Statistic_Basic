#5.1

y<-c(98,99,98.6,97.6,97.7,98,98.2,97.3,96.5,97.9,96.9,96.7)

rep(x=1:3,each=4)
a<-factor(rep(x=1:3,each=4),labels=paste("a",1:3,sep=""));a
b<-factor(rep(x=1:4,times=3),labels=paste("a",1:4,sep=""));b       

dat<-data.frame(y,a,b)
str(dat)

colors()
boxplot(y~a,data=dat,col=colors()[c(10,20,30)],xlab="가열온도",ylab="강도",main="플라스틱 제품강도")
boxplot(y~b,data=dat,col=colors()[c(10,20,30,40)],xlab="가열온도",ylab="강도",main="플라스틱 제품강도")

fit<-aov(y~a+b,data=dat)
anova(fit)

qf(p=0.01,df1=2,df2=6,lower.tail = FALSE)
pf(q=18.4286,df1=2,df2=6,lower.tail = FALSE)

pairwise.t.test(y,a,p.adj="none")


#5.2
#표 5-6
y<-c(-1,-5,-6,-1,-1,-8,-1,5,2,11,-1,13,1,2,-4,1,6,1,-2,-3,-3,5,-5,4,6)
row<-factor(rep(x=1:5,each=5));row
col<-factor(rep(1:5,5));col
trt<-factor(c(1,2,3,4,5,2,3,4,5,1,3,4,5,1,2,4,5,1,2,3,5,1,2,3,4))

fit<-aov(y~row+col+trt)
anova(fit)

qf(p=0.01,4,12,lower.tail = F)
pf(q=7.7344,4,12,lower.tail = F)


scores<-c(dat1,dat2);scores
groups<-rep(c("dat1","dat2"),c(10,10));groups
df<-data.frame(groups,scores)
str(df)
boxplot(scores ~ groups,data=df)

t.test(scores~groups,var.equal=T)
objects()
######

plot(density(dat1),lty=1,ylim=c(0,1.5),col=4,lwd=2)
lines(density(dat2),lty=2,col=2)

boxplot(dat1,dat2,ylab='약효',names=c("dat1","dat2"),main="생산 직후와 1년 후의 약효",
        pars=list(boxwex=0.5),col=c(4,2),border=c(4,2))
?boxplot
?par
#######

t.test(x=dat1,y=dat2,var.equal = TRUE) # t검정 바로 해주는 시스템 # var.equal :첫번째 분산과 두번째 분산이 같다고 가정
args(t.test)
?t.test
#######

dat1<-c(10.2,10.5,10.3,10.8,9.8,10.6,10.7,10.2,10.0,10.1)
dat2<-c(9.8,9.6,10.1,10.2,10.1,9.7,9.5,9.6,9.8,9.9)
mean(dat1)
mean(dat2)
n1<-length(dat1)
n2<-length(dat2)
sp<-round(sqrt(((n1-1)*var(dat1)+(n2-1)*var(dat2))/(n1+n2-2)),3)
sp
t<-round((mean(dat1)-mean(dat2))/(sp*sqrt(1/n1+1/n2)),2)
t
n1+n2-2 #자유도 18
cval<-round(qt(p=0.025,df=18,lower.tail=FALSE),3) #아래쪽꼬리가 아닌 위쪽 꼬리만 계산
cval

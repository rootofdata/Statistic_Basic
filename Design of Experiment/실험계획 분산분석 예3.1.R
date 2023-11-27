a1<-c(1.93,2.38,2.20,2.25)
a2<-c(2.55,2.72,2.75,2.70)
a3<-c(2.40,2.68,2.32,2.28)
a4<-c(2.33,2.38,2.28,2.25)
wear<-c(a1,a2,a3,a4)
group<-factor(rep(c("a1","a2","a3","a4"),times=c(4,4,4,4)))
group
cbind(wear,group)

#sum(wear[group=="a1"])
#sum(wear[group=="a2"])
#sum(wear[group=="a3"])
#sum(wear[group=="a4"])
#대신 tapply 쓰면 됨
Ti<-tapply(wear,group,sum)
Ti
T<-sum(wear)
T
a<-4
r<-4
CT<-T^2/(a*r);CT
SST<-sum(wear^2)-CT;SST
SSA<-sum(Ti^2/r)-CT;SSA
SSE<-SST-SSA;SSE
#교과서 46페이지

dfT<-16-1;dfT #자유도
dfA<-4-1;dfA
dfE<-dfT-dfA;dfE

dat<-data.frame(wear,group)
str(dat)
#rm(wear,group)

boxplot(wear ~group)
boxplot(wear~group,data=dat)

objects()

fit<-aov(wear~group,data=dat)
anova(fit)

bartlett.test(wear~group,data=dat)


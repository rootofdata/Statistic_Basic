a1<-c(97,67,42,125,25,92,105,86,27,43,45,59,53,21)
a2<-c(125,155,67,96,256,47,310,236,220,352,142,190)
a3<-c(142,256,310,440,495,510,320,396,196)
a4<-c(167,655,220,540,780)
(A1<-mean(a1))
(A2<-mean(a2))
(A3<-mean(a3))
(A4<-mean(a4))
v1<-var(a1)
v2<-var(a2)
v3<-var(a3)
v4<-var(a4)
N1<-86
N2<-72
N3<-52
N4<-30
Y1<-N1*(N1-length(a1))*var(a1)/length(a1)
Y2<-N2*(N2-length(a2))*var(a2)/length(a2)
Y3<-N3*(N3-length(a3))*var(a3)/length(a3)
Y4<-N4*(N4-length(a4))*var(a4)/length(a4)
Y<-(Y1+Y2+Y3+Y4)/(240*240)
Y
plot(a1)
X1<-c(a1,a2,a3,a4)
X2<-c(A1,A2,A3,A4)
X3<-c(sqrt(var(a1)),sqrt(var(a2)),sqrt(var(a3)),sqrt(var(a4)))
v<-c(v1,v2,v3,v4)
barplot(v,col=c(1,2,3,4),beside=T,names=c("Strat1","Strat2","Strat3","Strat4"))
legend(300,300,names(swiss),cex=0.8)
boxplot(X1)
mean(A1,A2,A3,A4)
plot(density(a1),lty=1,ylim=c(0,800))
barplot(v1,v2,v3,v4,ylab="acres",main="비교",col = c(1,2,3,4))
barplot(v1,v2,v3,v4,ylab="acres",names=c("Stratum1","Stratum2","Stratum3","Stratum4"),main="4구역의 비교")

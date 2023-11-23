#7.3

y<-c(-1,5,9,11,0,3,4,8,-1,-9,1,-5,-9,-13,5,-4)
a<-rep(rep(c(-1,1),each=4),2);a
b<-rep(rep(c(-1,1),each=2),4);b
c<-rep(rep(c(-1,1),each=1),8);c
d<-rep(rep(c(-1,1),each=8),1);d
#-1,1로 코딩: orthogonal structure in the design matrix

par(mfrow=c(2,2))
boxplot(y~a)
boxplot(y~b) #b,d가 눈대중으로 봤을때 차이가 있음
boxplot(y~c)
boxplot(y~d)

fit<-aov(y~(a+b+c+d)^3)
anova(fit)
#fit<- update(fit, .~.-a:b:c)
fit<-step(fit,direction = "backward")
anova(fit) #aic를 가장 값을 작게 만드는 
#유요한 상호작용 효과와 주효과만 포함
fit<-aov(y~a+b+c+d+c:d+a:b:d)
anova(fit)
#만약 0,1로 코딩을 하면 어떻게 되는지 확인해 보세요

#A1,B1,D0 수준이 최적으로 판단됨.
MT<-mtcars

str(MT)
summary(MT)
MT$vs<-as.factor(MT$vs)
MT$am<-as.factor(MT$am)
MT$gear<-as.factor(MT$gear)
MT$carb<-as.factor(MT$carb)

plot(MT)

lm(data=MT,mpg ~cyl+disp+hp) #mpg�� ����������, cyl disp hp�� ��������
fit1<-lm(data=MT,mpg ~.) #�������� ��������(.���� ��ü�� ��Ÿ��)
summary(fit1) #t value, pr�� �ؼ� -> vs1 : vs���� 1�϶�,
#F-statistic �� ���� r����� ������ ����ȵ�,, #���߰����� ����
#���߰����� �ذ�å - ����Ѱ� ���� (��� ������?)

library(car)
vif(fit1)

fit2<-step(fit1,direction = "backward") #AIC�� ������ ������. 
fit2$model
fit2
step(fit1,direction = "both") #both�� ���⸸�ҰԾƴ϶�(backward) �����ֱ⵵ �Ѵ�.
fit0<-lm(data=MT,mpg~1) #�ƹ��͵� ���Ե������� ����
#������ ���Ҷ� 1�� �� ->mpg�� ��� 

form1<-formula(fit1)

fit3<-step(fit0,scope=form1,direction = "forward")
#���ϸ鼭 �����ϱ�
step(fit0,scope=form1,direction = "backward")
AIC(fit2)
 #fit2�� ����� aic�� �� �����Ƿ� fit2�� ���ϴ°� ����.
AIC(fit3)
vif(fit2) #fit2�� ���ϰ� �� ���� 4�� ���� �����ϱ�, ������.
summary(fit2)# fit2�� ���
#p value �� ����. r-squared �� 85����, adjusted r squared 83.36 ��, �������� p-value�� ����- �����ϴ�.
plot(fit2)

res<-residuals(fit2)

fit2$residuals(fit)

shapiro.test(res)

durbinWatsonTest(res)

mpg~cyl+hp
mpg~.-1-hp #��� �ִµ� hp�� ����׸� ����
mpg~hp*cyl 

MT$log_hp<-log(MT$hp,10)

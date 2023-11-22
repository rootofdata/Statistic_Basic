library(readxl)

#������ �ҷ�����
gasM<-read_xlsx(file.choose(),1)
gasM<-as.data.frame(gasM)
#������ Ȯ��
str(gasM)
summary(gasM)
#����ȸ��
fit<-lm(data=gasM,Y~.)
summary(fit)

#���߰����� Ȯ��
plot(gasM)

###������
cor(gasM)
library(car)
vif(fit)
#���߰����� �ذ�,���� ����

###�����
fit_back<-step(fit,direction="backward")
summary(fit_back)

fit<-lm(data=gasM,Y~.)
summary(fit)

###������
fit_forward<-step(fit0,direction = "forward",scope = form_full)
fit0<-lm(data=gasM,Y~1)
summary(fit0)

form_full<-formula(fit)
summary(fit_forward)

###both
step(fit,direction = "both")
step(fit0,direction = "both",scope=form_full)

summary(fit_back)
summary(fit_forward)

AIC(fit_back)
AIC(fit_forward) ##���� ������, �ٸ��ٸ� aic���� �� ���� ���� ���� ��.

####��������
vif(fit_back)

#### ������ȯ
logT<-read.xlsx(file.choose(),1)
head(logT)
str(logT)

#������
plot(logT$t,logT$N_t)
#������
logT$logN<-log(logT$N_t)
str(logT)

#�α׻�����
plot(logT$t,logT$logN)
#ȸ�͸���
fit_L<-lm(data=logT,t~logN)
fit_N<-lm(data=logT,t~N_t)
summary(fit_L)
plot(fit_L)

#####
plot(x,y,type='l',col='blue',lwd=3,ylim)



#########
job<-read_xlsx(file.choose(),1)
str(job)
job$Education<-as.factor(job$Education)
job$Management_job<-as.factor(job$Management_job)

str(job)
summary(job)

####�������� �з�
mod1<-lm(data=job,Salrary~.)
summoary(mod1)
mod2<-lm(data=job,Salrary~.+Education*Management_job)
summoary(mod2)
##wage
wage<-read_xlsx(file.choose(),1)
str(wage)
wage$sex<-as.factor(wage$sex)
wage$nonwh<-as.factor(wage$nonwh)
wage$hisp<-as.factor(wage$hisp)

summary(wage)

##ȸ�ͺм�
mod3<-lm(data=wage,wage~.-OBS)
summary(mod3)

library(readxl)

#데이터 불러오기
gasM<-read_xlsx(file.choose(),1)
gasM<-as.data.frame(gasM)
#데이터 확인
str(gasM)
summary(gasM)
#선형회귀
fit<-lm(data=gasM,Y~.)
summary(fit)

#다중공선성 확인
plot(gasM)

###상관계수
cor(gasM)
library(car)
vif(fit)
#다중공선성 해결,변수 선택

###백워드
fit_back<-step(fit,direction="backward")
summary(fit_back)

fit<-lm(data=gasM,Y~.)
summary(fit)

###포워드
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
AIC(fit_forward) ##둘이 같지만, 다르다면 aic값이 더 작은 것이 좋은 것.

####최종모형
vif(fit_back)

#### 변수변환
logT<-read.xlsx(file.choose(),1)
head(logT)
str(logT)

#산점도
plot(logT$t,logT$N_t)
#새변수
logT$logN<-log(logT$N_t)
str(logT)

#로그산점도
plot(logT$t,logT$logN)
#회귀모형
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

####관리직과 학력
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

##회귀분석
mod3<-lm(data=wage,wage~.-OBS)
summary(mod3)


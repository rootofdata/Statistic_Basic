MT<-mtcars

str(MT)
summary(MT)
MT$vs<-as.factor(MT$vs)
MT$am<-as.factor(MT$am)
MT$gear<-as.factor(MT$gear)
MT$carb<-as.factor(MT$carb)

plot(MT)

lm(data=MT,mpg ~cyl+disp+hp) #mpg를 반응변수로, cyl disp hp를 설명변수
fit1<-lm(data=MT,mpg ~.) #나머지를 설명변수(.쓰면 전체를 나타냄)
summary(fit1) #t value, pr로 해석 -> vs1 : vs값이 1일때,
#F-statistic 모델 유의 r스퀘어도 높지만 쓰면안됨,, #다중공선성 문제
#다중공선성 해결책 - 비슷한거 빼기 (어떻게 고를까?)

library(car)
vif(fit1)

fit2<-step(fit1,direction = "backward") #AIC가 작으면 좋은것. 
fit2$model
fit2
step(fit1,direction = "both") #both는 빼기만할게아니라(backward) 더해주기도 한다.
fit0<-lm(data=MT,mpg~1) #아무것도 포함되지않은 변수
#절편만 구할때 1만 씀 ->mpg의 평균 

form1<-formula(fit1)

fit3<-step(fit0,scope=form1,direction = "forward")
#더하면서 시작하기
step(fit0,scope=form1,direction = "backward")
AIC(fit2)
 #fit2의 결과가 aic가 더 작으므로 fit2를 택하는게 낫다.
AIC(fit3)
vif(fit2) #fit2를 택하고 각 값이 4를 넘지 않으니까, 괜찮다.
summary(fit2)# fit2의 결과
#p value 다 낮다. r-squared 약 85프로, adjusted r squared 83.36 굿, 마지막줄 p-value가 낮다- 유의하다.
plot(fit2)

res<-residuals(fit2)

fit2$residuals(fit)

shapiro.test(res)

durbinWatsonTest(res)

mpg~cyl+hp
mpg~.-1-hp #모두 넣는데 hp랑 상수항만 빼기
mpg~hp*cyl 

MT$log_hp<-log(MT$hp,10)


install.packages('readxl')


library(readxl)
                #파일 경로                                                 데이터 시트번호
adv<-read_xlsx('C:/Users/dudtj/OneDrive/Desktop/회귀분석1/광고비와 매출액.xlsx',1)
#adv<-read_xlsx(file.choose(),1)
adv<-read.csv(file.choose())
#불러온 데이터 확인
str(adv)
summary(adv)
head(adv,5)
adv<-as.data.frame(adv)
adv
str(adv)
colnames(adv)<-c('광고비','매출액')
head(adv,5)

na.omit(adv) # NA가 들어있는 모든 것 다 없앰
plot(adv$광고비,adv$매출액,xlab='매출액',ylab='ttt')

plot(data=adv,매출액~광고비)

#### 단순선형회귀분석

#lm()함수 이용
fit<-lm(data=adv,매출액~광고비) # lm(데이터이름,회귀식)
fit
summary(fit) #p-값이 2.068e-08(10의 -8승이다) 이거 굉장히 작아서 유의하다. F값도 10넘어가면 유의함.
#r squared 광고비랑 매출액이 98%정도 설명된다.

residuals(fit)
r<-residuals(fit)
fitted.values(fit) #적합도도 구할 수 있다.

#잔차에 대한 가정 확인
plot(fit)


### 잔차의 정규성 shapiro-wilk test
shapiro.test(r) #귀무가설은 잔차가 정규분포를 따른다.
#대립가설은 잔차가 정규분포를 따르지않는다 
#p value가 크니까 정규분포를 따른다. (0.3247)


####잔차의 독립성 durbin-watson test
#car 패키지 필요
library(car)
durbinWatsonTest(r)
# 통계량을 줘서 직접 해석 2에 가까우면 잔차가 독립, 0이나 4에 가까워지면 잔차는 독립이 아님. - 결과 독립처럼 나옴.

##산점도에 회귀직선 추가
plot(data=adv,매출액~광고비,pch='*',col="blue")

abline(fit,lwd=2,col="red",lty=2)
#     선의 굵기 선의 색깔 선의 종류

##예측하기

newdata<-data.frame(광고비=c(6,10))

predict(fit,newdata)

predict(fit,newdata,se.fit = T)
predict(fit,newdata,interval='prediction') # interval 정해줘서 하기

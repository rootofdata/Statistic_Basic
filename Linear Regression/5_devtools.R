install.packages("devtools")
library(devtools)
install_github("regbook/regbook")
library(regbook)

english2$method <-relevel(english2$method,ref="C")
fit<-lm(postscore~method+prescore,english2)
summary(fit)
fit2<-lm(postscore~method,english2) #prescore 제외 (1)
summary(fit2)
#유의한 차이 나지 않는다.

#(2)
Y=22.677+3.055Za-1.875Zb+0.740X1
A=22.677+3.055Za+0.740X1 ->25.732+0.740X1
B=22.677+1.875Zb+0.740X1 ->24.552+0.740X1
C=22.677+0.740X1

#(3)
fit3<-lm(postscore~method*prescore,english2)
summary(fit3)

#7.14
#(1)
hweight$gender<-relevel(hweight$gender,ref="M")
fit<-lm(weight~height*gender,hweight)
summary(fit) 
#(2)
0.80291height+21.17368genderF-0.16592height:genderF -70.02715
남 0.80291height-70.02715 #남자
여 0.63699height-48.85347 #여자
pr 0.136 기울기 차이 없다.

#7.17
normaldensity
fit1<-lm(y~x,normaldensity)
summary(fit1)
fit2<-update(fit1,.~.+I(x^2))
summary(fit2)
fit3<-update(fit2,.~.+I(x^3))
summary(fit3)
fit4<-update(fit3,.~.+I(x^4)) #x는 유의하지 않게 된다. x^4는 유의 4차까지 적당? 혹은 3차까지 적당한가. ->3차까지 적당한듯.
summary(fit4)
fit5<-update(fit4,.~.+I(x^5)) 
summary(fit5) #유의하지않다.

fit<-lm(y~poly(x,4,raw=TRUE),normaldensity)
summary(fit)

#7.19
#(1)
install.packages(rsm)
factorial
library(rsm)
y.rsm<-rsm(y~FO(x1,x3),factorial)
y.rsm
#(2)
summary(y.rsm)
y=3.5+1x1+1.25x3+0.25x1x3(0.25는 의미없다)
xs(y.rsm)
contour(y.rsm,~x1+x3,image=TRUE)
y.rsm$b
y.rsm$B
xs(y.rsm)

#7.21
wrapper.rsm<-rsm(y~SO(x1,x2,x3),wrapper)
wrapper.rsm
summary(wrapper.rsm)

library(regbook)
data("hweight")
male<-subset(hweight,gender=="M")
hist(male$height)
hist(male$height,probability = T) # 빈도가 아닌 비율로 표현
lines(density(male$height))
histf(male$height)
y<-male$height
summary(y)
quantile(y)
boxplot(height~gender,hweight,horizontal = T)
n<-length(y)
mean(y)+qt(c(0.025,0.975),n-1)*sd(y)/sqrt(n)
with(male,cor.test(height,weight))

plot(weight~height,col=gender,data=hweight)

plot(foot)
abline(hweight)

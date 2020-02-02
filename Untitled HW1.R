data('fruitfly', package = 'faraway')
dim(fruitfly)
summary(fruitfly)

knitr:: kable(summary(fruitfly), align = 'c', digits = 2)

gm = glm(thorax ~ longevity + activity, family = Gamma(link = 'log'), data = fruitfly)

shape = 1/summary(gm)$dispersion
scale = exp(gm$coef["(intercept)"])/shape

hist(fruitfly$thorax, xlab = 'Thorax', main = "Histogram of fruitlies' thorax")
hist(fruitfly$longevity, xlab = 'longevity', main = "Histogram of fruiflies' longevity", prob = T, ylim = c(0, 0.025) )


plot(fruitfly$activity, fruitfly$longevity, xlab = 'activity', ylab = 'longevity',
     main = 'Longevity of flies under different activity levels')

plot(fruitfly$thorax, fruitfly$longevity, xlab = 'activity', ylab = 'longevity',
     main = 'Longevity of flies under different activity levels')

plot (longevity ~ thorax, fruitfly, pch=unclass (activity))
legend(0.63, 100, levels(fruitfly$activity), pch = 1.5)


gm1 <- glm(longevity ~ thorax * activity, family = Gamma(), data = fruitfly)
par(mfcol = c(2,2))

plot(gm1)
summary(gm)$dispersion
anova(gm)







data('fruitfly', package = 'faraway')
dim(fruitfly)
knitr:: kable(summary(fruitfly), align = 'c', digits = 2, caption = 'Summary of fruitfly data')

#hist
hist(fruitfly$longevity, xlab = 'longevity', main = "Histogram of fruiflies' longevity", prob = T, ylim = c(0, 0.025) )

#normalize thorax
NormThorax = ((fruitfly$thorax - mean(fruitfly$thorax)) / sqrt(var(fruitfly$thorax)))
gm = glm(longevity ~ NormThorax + activity, family = Gamma(link = 'log'), data = fruitfly)
summary(gm)
knitr::kable(summary(gm)$coef, digits = 3, caption = 'Coefftients of Fixed GM')


shape = 1/summary(gm)$dispersion
scale = exp(gm$coef["(Intercept)"]) / shape
hist(fruitfly$longevity, xlab = 'longevity', main = "Histogram of fruiflies' longevity", prob = T, ylim = c(0, 0.05) )
x = seq(0, 120, len = 1000)
x
lines(x, dgamma(x, shape = shape, scale = scale), col = 'red')





ID<-c(1,2,3,4)
name<-c("A","B","C","D")
score<-c(60,70,80,90)
sex<-c("M","F","M","M")
student1<-data.frame(ID,name)
student2<-data.frame(score,sex)
student1
student2
total_student2<-cbind(student1,student2)
total_student2

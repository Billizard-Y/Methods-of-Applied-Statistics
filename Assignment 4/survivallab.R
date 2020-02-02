
data("cricketer", package = "DAAG")

library('INLA')
cricketer$decade = (cricketer$year - 1850)/10
cricketer$lifeC = cricketer$life / 100

 cFitI = inla(inla.surv(lifeC, acd) ~ 
                decade + left, data=cricketer, 
              family='weibullsurv',
               control.family = list(variant=1, 
                       hyper=list(alpha = list(
       prior = 'normal', param = 
         c(log(7.5), (2/3)^(-2))
     ))
    ), control.compute = list(config=TRUE))

 knitr::kable(cFitI$summary.fixed[,1:5], digits=3)
 
 xSeq = seq(0,2000, len=1001)
 densHaz = Pmisc::sampleDensHaz(fit = cFitI, 
               x = xSeq,
   n = 50, scale = 100)
 
 theScale = exp(-cFitI$summary.fixed['(Intercept)',
                                '0.5quant'])
 theShape = cFitI$summary.hyper[1,'0.5quant']
 
 theScaleLeft = exp(-sum(cFitI$summary.fixed[
   c('(Intercept)','leftleft'),
   '0.5quant']))
 
 
 xYears = seq(0,1000,len=201)
 xCenturies = xYears / 100
 plot(xCenturies, dweibull(xCenturies,
                           shape = theShape,
                           scale = theScale))
 
' =(𝜅/𝜆) ⋅ (𝑡/𝜆)𝜅−1
''
 'l
xCenturies = seq(0, 1, len=201)
xYears = seq(0,100,len=201)
theScaleYears = theScale * 100
theScaleLeftYears = theScaleLeft * 100
theHaz = (theShape / theScaleYears)o* (
  xYears / theScaleYears)^(theShape-1) 

theHazLeft = (theShape / theScaleLeftYears) * (
  xYears / theScaleLeftYears)^(theShape-1) 

plot(xYears, 10^5 *theHaz,type='l',
     ylim = c(0, 150),
     xlab='age', ylab='deaths/100,000')
lines(xYears, 10^5*theHazLeft, col='red')
(xSeq, densHaz[,'dens',], 
         type='l', lty=1,
         col='#00000030')

  matplot(xSeq, densHaz[,'cumhaz',], 
         type='l', lty=1,
         ylim = c(0, 5),
         col='#00000030')
 library('survival')

  hazEst = survfit(Surv(life, acd) ~ left, data=cricketer)
plot(hazEst, fun='cumhaz', col=c('blue','red'))
 matlines(xSeq, densHaz[,'cumhaz',],
          col='#00000030', lty=1) 
 
# https://stat.ethz.ch/R-manual/R-devel/library/survival/html/flchain.html
data('flchain', package='survival')
hist(flchain$kappa)
hist(flchain$lambda)

quantile(flchain$futime)
x = flchain[flchain$futime > 0, ]

summary(lm(futime ~ age + sex + kappa + lambda, data=x))$coef


summary(survival::survreg(Surv(futime, death) ~ sex, data=x, dist='weibull'))
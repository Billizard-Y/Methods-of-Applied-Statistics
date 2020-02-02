#' load the smoking data
#+ smokeData
#
#******************** Understanding what the factors are in important for HW1 **************************
#
smokeUrl = 'http://pbrown.ca/teaching/appliedstats/data/smoke.RData'
(smokeFile = tempfile(fileext='.RData'))
download.file(smokeUrl, smokeFile)
(load(smokeFile))
dim(smoke)
#'
#' 
#+ exploreSmoke
smoke[1:5,c('Age','Sex','Grade','RuralUrban','Race', 'Tried_cigarette_smkg_even')]
smokeFormats[smokeFormats$colName == 'Tried_cigarette_smkg_even', ]
smoke$everSmoke = factor(smoke$Tried_cigarette_smkg_eve, levels=1:2, labels=c('yes','no'))
table(smoke$Grade, smoke$Age, exclude=NULL)
table(smoke$Race, smoke$everSmoke, exclude=NULL)
#' 
#' nine year olds look suspicious
#' get rid of missings and age 9
#' to make the dataset smaller (removing the useless datas)
#+ smokeSub
smokeSub = smoke[smoke$Age != 9 & !is.na(smoke$Race) &
	!is.na(smoke$everSmoke), ]
dim(smokeSub)
#'
#' 
#+  
smokeAgg = reshape2::dcast(smokeSub,
	Age + Sex + Race + RuralUrban ~ everSmoke,
	length)
dim(smokeAgg)
smokeAgg = na.omit(smokeAgg)
dim(smokeAgg)

smokeAgg[which(smokeAgg$Race == 'white' & 
	smokeAgg$Sex == 'M' & smokeAgg$RuralUrban == 'Urban'),] #setting the base line catagory 
smokeAgg$total = smokeAgg$no + smokeAgg$yes
smokeAgg$prop = smokeAgg$yes / smokeAgg$total
#'
#' 
#+ smokeExplPlot
Spch = c('M' = 3, 'F'=1) #M for male and F for female
Scol = RColorBrewer::brewer.pal(nlevels(smokeAgg$Race), 'Set2')
names(Scol) = levels(smokeAgg$Race)
plot(smokeAgg$Age, smokeAgg$prop, pch = Spch[as.character(smokeAgg$Sex)],
	col = Scol[as.character(smokeAgg$Race)])
legend('topleft', fill=Scol, legend=names(Scol))
legend('left', pch=Spch, legend=names(Spch))
#' 
#' Which races smoke the least?
#' ... age is a confounder
#' ... as is urban/rural.  
#+ smokeModel 
smokeAgg$y = cbind(smokeAgg$yes, smokeAgg$no)
smokeFit = glm(y ~ Age + Sex + Race + RuralUrban, 
	family=binomial(link='logit'), data=smokeAgg)
knitr::kable(summary(smokeFit)$coef, digits=3)
#'
#' 
#' Intercept is age zero
#+ smokeFit2
smokeAgg$ageC = smokeAgg$Age - 15 # center Age so intercept is age 15
smokeFit2 = glm(y ~ ageC + Sex + Race + RuralUrban, 
	family=binomial(link='logit'), data=smokeAgg) # fitting a binomial model, should get a matrix 
knitr::kable(summary(smokeFit2)$coef, digits=3) 
#'
#' 
#' convert to baseline prob and odds 
#+ smokeConvert
smokeTable = as.data.frame(summary(smokeFit2)$coef) # converting a matrix into data frame
smokeTable$lower = smokeTable$Estimate - 2*smokeTable$'Std. Error'
smokeTable$upper = smokeTable$Estimate + 2*smokeTable$'Std. Error'
smokeOddsRatio = exp(smokeTable[,c('Estimate','lower','upper')]) #the first row of this chart is odds
smokeOddsRatio["ageC", "Estimate"] # the odds of full sample
1/ smokeOddsRatio["ageC", "Estimate"] # the probability (1/odds)
rownames(smokeOddsRatio)[1] = 'baseline prob'
smokeOddsRatio[1,] = smokeOddsRatio[1,]/(1+smokeOddsRatio[,1])
smokeOddsRatio
#'
#' make row names nicer
#+ newNames 
rownames(smokeOddsRatio) = gsub("Race|RuralUrban|C$", "",
                                rownames(smokeOddsRatio) )
rownames(smokeOddsRatio) = gsub("SexF", "Female",
                                rownames(smokeOddsRatio))
knitr::kable(smokeOddsRatio, digits=3)
#'
#+ smokeFitted
toPredict = smokeAgg[smokeAgg$RuralUrban == 'Urban', ]
smokePred = as.data.frame(predict(smokeFit2, toPredict,  se.fit=TRUE))
smokePred$lower = smokePred$fit - 2*smokePred$se.fit #lower bound
smokePred$upper = smokePred$fit + 2*smokePred$se.fit #upper bound
smokePredExp = exp(smokePred[,c('fit','lower','upper')])
smokePredProb = smokePredExp / (1+smokePredExp)
#'
#+ plotFitted 
plot(toPredict$Age, smokePredProb$fit, 
	pch = Spch[as.character(toPredict$Sex)],
	col = Scol[as.character(toPredict$Race)],
	xlab='age', ylab='prob')
legend('topleft', fill=Scol, legend=names(Scol))
legend('left', pch=Spch, legend=names(Spch))

#'

#' asian males with error bars
#+ plotAsian
isAsianMale = toPredict$Sex == 'M' & toPredict$Race == 'asian'
matplot(
	toPredict[isAsianMale, 'Age'],
	smokePredProb[isAsianMale, c('fit','lower','upper')],
	type='l', lty=c(1,2,2), lwd=3, col=c('black','grey','grey'), xlab = 'Age', ylab = 'prob'
	)





# *****************REFITTING THE MODEL***********************
#' 
#' #+ smokeFit2
smokeAgg$ageC = smokeAgg$Age - 15 # center Age so intercept is age 15
smokeFit2INT = glm(y ~ ageC * Sex * Race + RuralUrban, 
                family=binomial(link='logit'), data=smokeAgg) # REfitting a binomial model, should get a matrix 
knitr::kable(summary(smokeFit2INT)$coef, digits=3) 
#'
#'#'
#' 
#' convert to baseline prob and odds 
#+ smokeConvert
smokeTable = as.data.frame(summary(smokeFit2INT)$coef) # converting a matrix into data frame
smokeTable$lower = smokeTable$Estimate - 2*smokeTable$'Std. Error'
smokeTable$upper = smokeTable$Estimate + 2*smokeTable$'Std. Error'
smokeOddsRatio = exp(smokeTable[,c('Estimate','lower','upper')]) #the first row of this chart is odds
smokeOddsRatio["ageC", "Estimate"] # the odds of full sample
1/ smokeOddsRatio["ageC", "Estimate"] # the probability (1/odds)
rownames(smokeOddsRatio)[1] = 'baseline prob'
smokeOddsRatio[1,] = smokeOddsRatio[1,]/(1+smokeOddsRatio[,1])
smokeOddsRatio
#'
#' make row names nicer
#+ newNames 
rownames(smokeOddsRatio) = gsub("Race|RuralUrban|C$", "",
                                rownames(smokeOddsRatio) )
rownames(smokeOddsRatio) = gsub("SexF", "Female",
                                rownames(smokeOddsRatio))
knitr::kable(smokeOddsRatio, digits=3)
#'
#+ smokeFitted
toPredict = smokeAgg[smokeAgg$RuralUrban == 'Urban', ]
smokePred = as.data.frame(predict(smokeFit2INT, toPredict,  se.fit=TRUE))
smokePred$lower = smokePred$fit - 2*smokePred$se.fit #lower bound
smokePred$upper = smokePred$fit + 2*smokePred$se.fit #upper bound
smokePredExp = exp(smokePred[,c('fit','lower','upper')])
smokePredProb = smokePredExp / (1+smokePredExp)
#'
#+ plotFitted 
plot(toPredict$Age, smokePredProb$fit, 
     pch = Spch[as.character(toPredict$Sex)],
     col = Scol[as.character(toPredict$Race)],
     xlab='age', ylab='prob')
legend('topleft', fill=Scol, legend=names(Scol))
legend('left', pch=Spch, legend=names(Spch))

#'

#' asian males with error bars
#+ plotAsian
isAsianMale = toPredict$Sex == 'M' & toPredict$Race == 'asian'
matplot(
  toPredict[isAsianMale, 'Age'],
  smokePredProb[isAsianMale, c('fit','lower','upper')],
  type='l', lty=c(1,2,2), lwd=3, col=c('black','grey','grey'), xlab = 'Age', ylab = 'prob')
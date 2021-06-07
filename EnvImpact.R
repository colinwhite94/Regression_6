library(car)     ## avPlots, vif
library(MuMIn)   ##  
install.packages("foreach")
library(bestglm) ##You can also use regsubsets() in library(leaps) but I like this one
library(lmtest)   ## bptest
library(MASS)     ## stdres
library(car)      ## added-variable plots
library(normtest) 
library(knitr)


#########################################
## Read in EI Data and Make Transforms ##
setwd("~/Desktop/1A School/1A Winter 2021/STAT330/HW6")
ei = read.csv("gdp_sub_2.csv", header=TRUE)
head(ei)              
pairs(ei[,c(1:)])

##################
## Look at VIFs ##
##################
round(cor(log.ei[,c(16,7:15)]),2)

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### VIF(x_j) = 1/(1 - R^2 for covariate j using all other covariates)
##### VIF(\hat{\beta}_j) = 1/(1 - R^2 for covariate j using all other covariates)
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

lm.model = lm(y~.,data=ei)
car::vif(lm.model)
#kable(car::vif(lm.model))

with(log.ei,plot(log.Hydro,log.Nit,pch=19))


X = model.matrix(lm.model)
y = log.ei$AAMort
Xy = cbind(X,y)

var.selection = bestglm(log.ei,IC="BIC",method="exhaustive",
                        TopModels=10) # Y MUST BE THE LAST COLUMN!!!!
my.best.lm = var.selection$BestModel

plot(var.selection$Subsets$BIC,type="b",pch=19,xlab="# of Vars",ylab="BIC")
summary(var.selection$BestModel)
confint(var.selection$BestModel)

var.selection2 = bestglm(log.ei,IC="BIC",method="forward",TopModels=10)
plot(var.selection2$Subsets$BIC,type="b",pch=19,xlab="# of Vars",ylab="BIC")
summary(var.selection$BestModel)
summary(var.selection2$BestModel)

var.selection$BestModels$Criterion[1]
var.selection2$BestModels$Criterion[1]

##########################################
## Perform Forward Selection using AIC  ##
##########################################

var.selection = bestglm(ei,IC="AIC",
                        method="forward",TopModels=10)
plot(var.selection$Subsets$AIC,type="b",
     pch=19,xlab="# of Vars",ylab="AIC")
summary(var.selection$BestModel)

#####################################################
## Perform Backward Variable Selection using PMSE ##
#####################################################

var.selection = bestglm(log.ei,IC="CV",method="exhaustive",
                        TopModels=10,t=1000)

plot(0:15,var.selection$Subsets$CV,type="b",pch=19,xlab="# of Vars",ylab="CV")


# pdf("plot_whatever.pdf",width = 1)
# plot(0:15,var.selection$Subsets$CV,type="b",pch=19,xlab="# of Vars",ylab="CV")
# dev.off()

summary(var.selection$BestModel)

var.selection$BestModel

###################### LOOCV

var.selection.loocv = bestglm(log.ei,IC="LOOCV",method="exhaustive",TopModels=10)

summary(var.selection.loocv$BestModel)

var.selection$BestModel

##each of these should be linear
avPlots(my.best.lm,pch = 20,cex = 0.8) 

plot(my.best.lm$fitted.values,my.best.lm$residuals,pch=19, ylab="Standardized Residuals", xlab="Fitted values")
abline(a=0,b=0)

## equal variance

plot(my.best.lm$fitted.values,my.best.lm$residuals,pch=19)
abline(a=0,b=0)

plot(ei$CONFUC,my.best.lm$residuals,pch=19)
abline(a=0,b=0,col = "red")

bptest(my.best.lm)

## Normality

hist(stdres(my.best.lm),freq = FALSE, xlab="Standardized Residuals", ylab="Density", ylim=c(0,.4), main="Histogram of STD. Residuals")
curve(dnorm,from = -3,to = 4,add = TRUE)

qqnorm(stdres(my.best.lm))
abline(0,1)


jb.norm.test(stdres(my.best.lm),nrepl = 1e5)
ks.test(stdres(my.best.lm),"pnorm")

kable(confint(my.best.lm))


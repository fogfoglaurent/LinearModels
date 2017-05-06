attach(exempleB)
Y=cbind(y1,y2,y3)
f1=as.factor(f1)
f2=as.factor(f2)
attach(exempleB)

summary(Y)
tapply(y1,f1,mean)
tapply(y2,f1,mean)
tapply(y3,f1,mean)
tapply(y1,f2,mean)
tapply(y2,f2,mean)
tapply(y3,f2,mean)
tapply(y1,list(f1,f2),mean)
tapply(y2,list(f1,f2),mean)
tapply(y3,list(f1,f2),mean)
table(f1)
table(f2)
table(f1,f2)

library(Hmisc)
rcorr(Y)

fit.int=manova(Y~f1*f2)
summary.aov(fit.int)
summary(fit.int,test="Wilks") 
summary(fit.int,test="Hotelling-Lawley") 
summary(fit.int,test="Pillai")
summary(fit.int,test="Roy") 

options(contrasts=c("contr.SAS","contr.SAS"))
fit.lm.int=lm(Y~f1*f2)
summary(fit.lm.int)

p=nlevels(f1)*nlevels(f2)
p
n=length(y1)
n
E = (n-1)*cov(fit.int$residuals)
E
S = E/(n-p)
S
H = (n-1)*cov(fit.int$fitted.values)
H

library(Hmisc)
rcorr(fit.int$residuals)

# modèle additif
fit.add=manova(Y~f1+f2)
summary.aov(fit.add)
summary(fit.add,test="Wilks") 
summary(fit.add,test="Hotelling-Lawley") 
summary(fit.add,test="Pillai")
summary(fit.add,test="Roy") 

options(contrasts=c("contr.SAS","contr.SAS"))
fit.lm.add=lm(Y~f1+f2)
summary(fit.lm.add)

p=nlevels(f1)+nlevels(f2)-1
p
n=length(y1)
n
E = (n-1)*cov(fit.add$residuals)
S = E/(n-p)
S
H = (n-1)*cov(fit.add$fitted.values)
H
rcorr(fit.add$residuals)


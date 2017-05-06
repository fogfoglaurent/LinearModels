attach(exempleA)
Y=cbind(y1,y2,y3)
f=as.factor(f)
attach(exempleA)

summary(Y)
tapply(y1,f,mean)
tapply(y2,f,mean)
tapply(y3,f,mean)
table(f)

par(mfrow=c(1,3))
boxplot(y1~f, xlab="Facteur f", ylab="Variable réponse y1")
boxplot(y2~f,xlab="Facteur f", ylab="Variable réponse y2")
boxplot(y3~f,xlab="Facteur f", ylab="Variable réponse y3")
layout(1)

library(Hmisc)
rcorr(Y)

fitm=manova(Y~f)
summary.aov(fitm) 
# pour obtenir les R2 et les estimations des paramètres : pas forcément utile sur les modèles intermédiaires
summary(lm(Y~f))
# pour obtenir les résultats des tests multidimensionnels : par défaut, test de Pillai
summary(fitm) 
summary(fitm,test="Wilks") 
summary(fitm,test="Hotelling-Lawley") 
summary(fitm,test="Roy") 
summary(fitm,test="Pillai")

# pour obtenir les estimations des paramètres des ANOVAS sous le paramétrage SAS
options(contrasts=c("contr.SAS","contr.SAS"))
fit.lm=lm(Y~f)
summary(fit.lm)

# éléments donnés automatiquement dans SAS : intéressant par rapport aux éléments du cours, 
# mais pas spécialement utiles pour l'interprétation
p=nlevels(f)
p
n=length(y1)
n
E = (n-1)*cov(fitm$residuals)
S = E/(n-p)
S
H = (n-1)*cov(fitm$fitted.values)
H
# pour obtenir les valeurs propres de E-1.H
eigen(solve(E) %*% H)






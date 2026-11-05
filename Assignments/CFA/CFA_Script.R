library(psych)
library(lavaan)
library(lavaanPlot)
install.packages('semPlot')
library(semPlot)

getwd()
setwd('/Users/usq25002/Desktop/SEMPractice/CFA')
####ose####
#import dataset
ose <- read.csv("ose.csv")

ose<-na.omit(ose)
head(ose)
#specify the model
model1 <- 'OSE=~OSE1+OSE2+OSE3+OSE4+OSE5+OSE6+OSE7+OSE8'
#fit the model
fit1 <- cfa(model = model1, data = ose)

#evaluate the model
summary(fit1, fit.measures=TRUE,standardized=TRUE) 

modificationIndices(fit1,sort=1)

## See if model improves

model2 <- 'OSE=~OSE1+OSE2+OSE3+OSE4+OSE5+OSE6+OSE7+OSE8;
OSE7~~OSE8'

fit1 <- cfa(model = model2, data = ose
            )
#evaluate the model
summary(fit1, fit.measures=TRUE,standardized=TRUE) 

#diagram
sem.graph(fit1)

lavaanPlot(xxx)
#respecify the model
model2 <- 'xxxx'
fit2 <- cfa(xxx)
summary(xxx) 

####bfi####
library(psych)
data(bfi)
#check the correlation matrix
cor(na.omit(bfi[,1:25]))
cor.plot(cor(na.omit(bfi[,1:25])))
head(bfi)
#specify the model
model3 <- 'xxx'
#fit the model
fit3 <- cfa(xxx)
#evaluate the model
summary(xxx) 
modificationIndices(fit3,sort=TRUE)
#respecify the model
model4 <- 'xxxx'
fit4 <- cfa(xxx)
summary(xxx) 
#diagram
lavaanPlot(xxx)

####iq####
#import the dataset
iq <- read.csv("iqitems-1.csv")
iq<-na.omit(iq)
describe(iq)
head(iq)

#specify the model
model4 <- 'reason=~reason.4+reason.16+reason.17+reason.19+letter.7+letter.33+letter.34+letter.58+matrix.45+matrix.46+matrix.47+matrix.55; rotate=~rotate.3+rotate.4+rotate.6+rotate.8'

#fit the model
# ordered says different items are ordinal
fit4 <- cfa(model = model4, data = iq,std.lv = TRUE, ordered = names(iq))
#evaluate the model
summary(fit4,fit.measures=TRUE,standardized=TRUE) 

modificationIndices(fit4)

## create graphs based on models you just fit 
semPaths(fit4)
#diagram
lavaanPlot(fit4, coefs = TRUE, stand=TRUE, covs=TRUE)

model5 <- '
factor1 =~'

#specify the second model
model6 <- 'factor1=~ reason.4 + reason.16 + reason.17 + reason.19; factor2=~ letter.7 + letter.34 + letter.58; factor3=~ matrix.45 + matrix.16 + matrix.47 + matrix.55
#fit the model
fit6<- cfa(xxx) 
#evaluate the model
summary(xxx)
#diagram
lavaanPlot(xxx)

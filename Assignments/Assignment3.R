library(lavaan)
#set working directory and load data weeks.csv
setwd("xxx")
weeks <- read.csv("xxx")
names(weeks)

####plot#####
library(reshape2) #use function melt to convert wide to long format
weeks.long<-melt(weeks, id.vars=c("region", "city","population"))
names(weeks.long)[c(4,5)]<-c("week","case")

library(ggplot2)
ggplot(weeks.long, aes(x = xxx, y = xxx, color = as.factor(xxx), group = xxx)) + 
  geom_point() + 
  geom_line() + 
  theme_classic(base_size = 18) + 
  theme(legend.position = "none") + 
  labs(title = "Trajectories", y = "Confirmed cases", x = "Week since May")

####LGCM####
model1='
#intercept
i =~ xxx
#slope
s =~ xxx
'
fit1<-growth(xxx)
summary(fit1,fit.measures=TRUE)

####add quadratic term####
model2='
#intercept
i =~ xxx
#slope
s1 =~ xxx
#quadratic term
s2 =~ xxx
'
fit2<-growth(xxx)
summary(fit2,fit.measures=TRUE)




library(ISLR)
library(leaps)
library(GGally)

setwd("~/Google Drive/Data Science/Courses/Fall 2015/Statistical Inference & Modelling/Homework/Group Project/")
#dataset <- readRDS("Crime.rds")
dataset <- read.csv('crime.csv')

#A dataframe containing:
#X: index
#Action: remove it
dataset$X <- NULL

#county: county identifier.

#year: from 1981 to 1987. PREDICTOR.
#crmrte: crimes committed per person.  TARGET VARIABLE.
#prbarr: 'probability' of arrest. PREDICTOR.
#prbcon: 'probability' of conviction. PREDICTOR.
#prbpris: 'probability' of prison sentenc. PREDICTOR.
#avgsen: average sentence, days. PREDICTOR.
#polpc: police per capita. PREDICTOR.
#density: people per square mile. PREDICTOR.
#taxpc:tax revenue per capita. PREDICTOR.
#region: one of 'other', 'west' or 'central'. PREDICTOR.
#smsm: 'yes' or 'no' if in SMSA. PREDICTOR.
#pctmin: percentage minority in 1980. PREDICTOR.
#wcon: weekly wage in construction. PREDICTOR.
#wtuc: weekly wage in trns, util, commun. PREDICTOR.
#wtrd: weekly wage in whole sales and retail trade. PREDICTOR.
#wfir: weekly wage in finance, insurance and real estate. PREDICTOR.
#wser: weekly wage in service industry. PREDICTOR.
#wmfg: weekly wage in manufacturing. PREDICTOR.
#wfed: weekly wage of federal emplyees. PREDICTOR.
#wsta: weekly wage of state employees. PREDICTOR.
#wloc: weekly wage of local governments employees. PREDICTOR.
#mix: offence mix: face-to-face/other. PREDICTOR.
#pctymle: percentage of young males. PREDICTOR.

#WE BUILD A CATEGORICAL VARIABLE WITH THE HIGHEST QUARTILE OF HIGH CRIME RATE
dataset$crmrte_cat <- rep('low/normal', length(dataset$crmrte))
dataset$crmrte_cat[dataset$crmrte  > median(dataset$crmrte)] <- "high"

#histogram of the response variable
par(mfrow=c(1,1) )
hist(crmrte)

hist(log(crmrte))

#we trace paired boxplots for all the variables
par(mfrow=c(2,3) )
boxplot(prbarr~crmrte_cat,data=dataset, main='prbarr')
boxplot(prbconv~crmrte_cat,data=dataset, main='prbconv')
boxplot(prbpris~crmrte_cat,data=dataset, main='prbpris')
boxplot(avgsen~crmrte_cat,data=dataset, main='avgsen')
boxplot(polpc~crmrte_cat,data=dataset, main='polpc')
boxplot(density~crmrte_cat,data=dataset, main='density')
#conclusions: density may have a predictive value for high crime rates

par(mfrow=c(2,3) )
boxplot(taxpc~crmrte_cat,data=dataset, main='taxpc')
boxplot(pctmin~crmrte_cat,data=dataset, main='pctmin')
boxplot(wcon~crmrte_cat,data=dataset, main='wcon')
boxplot(wtuc~crmrte_cat,data=dataset, main='wtuc')
boxplot(wfir~crmrte_cat,data=dataset, main='wfir')
boxplot(wser~crmrte_cat,data=dataset, main='wser')
#pctmin may have a predictive value

par(mfrow=c(2,3) )
boxplot(wmfg~crmrte_cat,data=dataset, main='wmfg')
boxplot(wfed~crmrte_cat,data=dataset, main='wfed')
boxplot(wsta~crmrte_cat,data=dataset, main='wsta')
boxplot(wloc~crmrte_cat,data=dataset, main='wloc')
boxplot(mix~crmrte_cat,data=dataset, main='mix')
boxplot(pctymle~crmrte_cat,data=dataset, main='pctymle')
#'wfed' and 'wmfg' may have a predictive value

#timeline of crime rate by year
y1 <- table(dataset[dataset$crmrte_cat=='high','year'])
y2 <- table(dataset[dataset$crmrte_cat!='high','year'])
barplot(t(cbind(y1,y2)), legend=c('high','low/normal'))
#no significance difference

ggpairs(dataset[,c( 'region','crmrte_cat')])
#candidate to test anova region west

ggpairs(dataset[,c( 'smsa','crmrte_cat')])
#smsa yes with high rate.

#anova test with region: west/ other.
# One Way Anova (Completely Randomized Design)
dataset$crmrte_cat <- as.factor(dataset$crmrte_cat)
dataset$region_w_nw <- rep('nw', length(dataset$crmrte_cat))
dataset$region_w_nw[dataset$region=='west'] <- 'w'
dataset$region_w_nw <- as.factor(dataset$region_w_nw)

fit.anova <- aov(crmrte ~ region_w_nw, data=dataset)
summary(fit.anova)
fit.anova
#compare the means
tapply(crmrte,region_w_nw, mean)

fit.anova2 <- aov(crmrte ~ region_w_nw + smsa, data=dataset)
summary(fit.anova2)
fit.anova2

fit.anova3 <- aov(crmrte ~ region_w_nw + smsa + region_w_nw:smsa, data=dataset)
summary(fit.anova3)
fit.anova3

#confidence interval for the median.
#The Sign Test and Associated Procedures
#Assumptions
#Suppose we have independent and identically distributed data X1, X2, . . .,
#independence OK
#identical distribution OK
#continuity OK


#sign test
med <- median(crmrte)
n <- length(crmrte)
x <- crmrte-med
pos <- sum(x>0) 
#p value 
1 - pbinom(pos, n, 1 / 2)

#confidence intervals
k<- 260+c(23,30,35,38) # probabilities 99%; 95%; 88%; 81%;
1 - 2 * pbinom(k, n, 1 / 2)
perc <- c(0.99,0.95,0.88,0.81)
for (i in 1:4){
  print(c(perc[i],'inf:',sort(crmrte)[k[i]], '--sup:',sort(crmrte)[n-k[i]+1],'/n'))
}

#wilcoxson
#Assumptions:
#independence
#identical distribution
#continuity
#symmetry
#to achieve the symetry we work with the log(crime rate)
crmrte_l <- log(crmrte)
#run the wilcox test
wct <- wilcox.test(crmrte_l, conf.int = TRUE)
exp(wct$conf.int)
wct$p.value

crmrtepairs(crime)
lm.fit = lm(crmrte ~ ., data = dataset)
test = sample(530,100)
test
xs = dataset[-which(names(dataset) %in% c("crmrte"))]
ys = dataset[which(names(dataset) %in% c("crmrte"))]
xs_test = xs[test,]
xs_train = xs[-test,]
ys_test = ys[test,]
ys_train = ys[-test,]
bestsubset=regsubsets(y ~ . , data = data.frame(y = ys_train, x = xs_train), nvmax = 23)

p = 23

set.seed(1)
val.train.errors = rep(NA, p)
val.test.errors = rep(NA, p)
x_cols = colnames(xs, do.NULL = FALSE, prefix = "x.")
colnames(xs) <- paste("x", x_cols, sep = ".")
x_cols = colnames(xs)

for (i in 1:p) {
  coefi = coef(bestsubset, id = i)
  pred = as.matrix(xs_test[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) 
                                                                  %in% x_cols]
  val.test.errors[i] = mean((ys_test - pred)^2)
}
which.min(val.test.errors)



for (i in 1:p) {
  coefi = coef(bestsubset, id = i)
  pred = as.matrix(xs_train[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) 
                                                                   %in% x_cols]
  val.train.errors[i] = mean((ys_train - pred)^2)
}
which.min(val.train.errors)

lm.2.fit = lm(crmrte ~  prbarr + prbconv + polpc + density + as.factor(region) + pctmin + wfed + pctymle, data = dataset)
summary(lm.2.fit)


#

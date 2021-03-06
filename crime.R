library(ISLR)
library(leaps)
library(glmnet)
library(rpart.plot)
library(caret)
library(MASS)
library(GGally)
library(grid)
library(ggplot2)
library(gridExtra)
library(splines)
library(caret)
library(knitr)

######################################################################
# Setup
######################################################################
load("ggthemes_data.rda")
source("theme_fivethirtyeight.R")
Crime <- read.csv("Crime.csv")
Crime = Crime[,-1] # Remove the index column

# Identify and remove the outliers found from later analyses
# fit.all <- lm(crmrte~bs(pctmin, knots=pctmin.knots) + log(prbconv) + log(polpc) + prbarr + density:county + prbarr:prbpris + pctmin:polpc + polpc:wfed + density:pctmin + density:pctymle + taxpc:wfed + region:wsta, data=Crime)
bad = c(440,353,437,439)
Crime = Crime[-bad,]

######################################################################
# Analysis of variables
######################################################################
#A dataframe containing:
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
Crime$crmrte_cat <- rep('low/normal', length(Crime$crmrte))
Crime$crmrte_cat[Crime$crmrte  > median(Crime$crmrte)] <- "high"
Crime$crmrte_cat <- ordered(Crime$crmrte_cat, levels = c("low/normal", "high"))


#histogram of the response variable

ggplot() + geom_density(aes(x = Crime$crmrte), alpha=.3, fill = "grey") + xlab("Crimes per person") +
  theme_fivethirtyeight() 
#histogram of the log of the response.
ggplot() + geom_density(aes(x = Crime$crmrte), alpha=.3, fill = "grey") + xlab("log(Crimes per person)") +
  scale_x_log10() +
  theme_fivethirtyeight()

#histogram of the log of the response.


#we trace paired boxplots for all the variables

boxplot_crmte_cat = function(variable, data = Crime){
  plot_df= data.frame(y = data[,variable], x = data$crmrte_cat)
  plot = ggplot(plot_df) + geom_boxplot(aes(y= y, x = x, fill=x)) + 
        theme_fivethirtyeight() + labs(title = variable, x= "crmrte_cat", y = variable )+
        scale_fill_fivethirtyeight("cyl")+
        theme(legend.position="none")
  
  return(plot)
}


grid.arrange(boxplot_crmte_cat(variable = "prbarr"), boxplot_crmte_cat("prbconv"), boxplot_crmte_cat("prbpris"),
          boxplot_crmte_cat("avgsen"), boxplot_crmte_cat("polpc"),boxplot_crmte_cat("density"), ncol=3)

#conclusions: density may have a predictive value for high crime rates

grid.arrange(boxplot_crmte_cat(variable = "taxpc"), boxplot_crmte_cat("pctmin"), boxplot_crmte_cat("wcon"),
             boxplot_crmte_cat("wtuc"), boxplot_crmte_cat("wfir"),boxplot_crmte_cat("wser"), ncol=3)

#pctmin may have a predictive value

grid.arrange(boxplot_crmte_cat(variable = "wmfg"), boxplot_crmte_cat("wfed"), boxplot_crmte_cat("wsta"),
             boxplot_crmte_cat("wloc"), boxplot_crmte_cat("mix"),boxplot_crmte_cat("pctymle"), ncol=3)

#'wfed' and 'wmfg' may have a predictive value

#timeline of crime rate by year

ggplot(Crime) + geom_bar(aes(x =factor(year), fill=crmrte_cat )) +
  scale_fill_fivethirtyeight("cyl") + theme_fivethirtyeight() + xlab("year")
#no significance difference

ggpairs(Crime[,c( 'region','crmrte_cat')])+
  theme_fivethirtyeight()
#candidate to test anova region west

ggpairs(Crime[,c( 'smsa','crmrte_cat')])+
  theme_fivethirtyeight()
#smsa yes with high rate.

#anova test with region: west/ other.
# One Way Anova (Completely Randomized Design)
Crime$crmrte_cat <- as.factor(Crime$crmrte_cat)
Crime$region_w_nw <- rep('nw', length(Crime$crmrte_cat))
Crime$region_w_nw[Crime$region=='west'] <- 'w'
Crime$region_w_nw <- as.factor(Crime$region_w_nw)

fit.anova <- aov(crmrte ~ region_w_nw, data=Crime)
summary(fit.anova)
fit.anova
#compare the means
tapply(Crime$crmrte,Crime$region_w_nw, mean)

#anova using two categorical values
fit.anova2 <- aov(crmrte ~ region_w_nw + smsa, data=Crime)
summary(fit.anova2)
fit.anova2

#anova using two categorical values and their interaction
fit.anova3 <- aov(crmrte ~ region_w_nw + smsa + region_w_nw:smsa, data=Crime)
summary(fit.anova3)
fit.anova3

#confidence interval for the median.
#The Sign Test and Associated Procedures
#Assumptions
#Suppose we have independent and identically distributed data X1, X2, . . .,
#independence OK
#identical distribution OK
#continuity OK

#A. sign test
med <- median(Crime$crmrte)
n <- length(Crime$crmrte)
x <- Crime$crmrte-med
pos <- sum(x>0) 
#p value 
1 - pbinom(pos, n, 1 / 2)

#confidence intervals
k<- 260+c(23,30,35,38) # probabilities 99%; 95%; 88%; 81%;
1 - 2 * pbinom(k, n, 1 / 2)
perc <- c(0.99,0.95,0.88,0.81)
for (i in 1:4){
  print(c(perc[i],'inf:',sort(Crime$crmrte)[k[i]], '--sup:',sort(Crime$crmrte)[n-k[i]+1],'/n'))
}

#B. wilcoxson
#Assumptions:
#independence
#identical distribution
#continuity
#symmetry
#to achieve the symetry we work with the log(crime rate)
crmrte_l <- log(Crime$crmrte)
#run the wilcox test
wct <- wilcox.test(crmrte_l, conf.int = TRUE)
exp(wct$conf.int)
wct$p.value

#fit some models to explore which variables are related to the target
test = sample(nrow(Crime), nrow(Crime)*.2)

#decision tree
fit <-rpart(crmrte_cat ~.,data=Crime[-test,-3])
prp(fit)
pred.fit <- predict(fit,newdata=Crime[test,-3])
pred.class <- rep('low/normal', length(Crime$crmrte_cat))
pred.class[pred.fit[,'high']>0.5] <- 'high'
confusionMatrix(pred.class,Crime$crmrte_cat)

#logistic regression
fit.reg <-glm(crmrte_cat ~.,data=Crime[-test,-3], family=binomial)
pred.fit <- predict(fit.reg,newdata=Crime[test,-3], type='response')
pred.class <- rep('low/normal', length(Crime$crmrte_cat))
pred.class[pred.fit>0.5] <- 'high'
confusionMatrix(pred.class,Crime$crmrte_cat)
fit.reg


######################################################################
# Testing and training set creation
######################################################################

test = sample(nrow(Crime), nrow(Crime)*.2)
excluded = c("crmrte", "crmrte_cat", "region_w_nw")
xs = Crime[-which(names(Crime) %in% excluded)]
ys = Crime[which(names(Crime) %in% c("crmrte"))]
xs_test = xs[test,]
xs_train = xs[-test,]
ys_test = ys[test,]
ys_train = ys[-test,]
p = dim(xs)[2]
lm.fit = lm(crmrte ~ ., data = Crime)



######################################################################
# Training and test errors for lm under best subset selection
######################################################################

bestsubset=regsubsets(y ~ ., data = data.frame(y = ys_train, x = xs_train), nvmax = p)
summary(bestsubset)

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
val.test.errors[which.min(val.test.errors)]

for (i in 1:p) {
  coefi = coef(bestsubset, id = i)
  pred = as.matrix(xs_train[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) 
                                                                   %in% x_cols]
  val.train.errors[i] = mean((ys_train - pred)^2)
}

which.min(val.train.errors)
val.train.errors[which.min(val.train.errors)]

######################################################################
# Logarithmic, interaction and polynomial fits
######################################################################

lm.2.fit = lm(crmrte ~  prbarr + prbconv + polpc + density + as.factor(region) + pctmin + wfed + pctymle, data = Crime)
summary(lm.2.fit)

fitnames = c("prbarr" , "prbconv" , "polpc" , "density" , "as.factor(region)" , "pctmin" , "wfed" , "pctymle", "crmrte")
pairs(Crime[names(Crime) %in% fitnames])

lm.3.fit = lm(crmrte ~  log(prbarr) + log(prbconv) + log(polpc) + density + as.factor(region) + pctmin + poly(wfed,3) + pctymle, data = Crime)
summary(lm.3.fit)

lm.4.fit = lm(crmrte ~  .*., data = Crime)
summary(lm.4.fit)

lm.5.fit = lm(crmrte ~  log(prbarr) + log(prbconv) + log(polpc) + density + as.factor(region) + pctmin + poly(wfed,3) + pctymle + .*., data = Crime)


######################################################################
# AIC
######################################################################

interaction.fit = stepAIC(lm.5.fit)

coefi = coef(interaction.fit)
pred = as.matrix(xs_test[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) 
                                                                %in% x_cols]
val.test.errors = mean((ys_test - pred)^2)
val.test.errors

######################################################################
# Lasso
######################################################################

formula = "~ log(prbconv) + log(polpc) + density + pctmin +  poly(wfed, 3) +
pctymle + county + year + prbarr + prbconv +   prbpris + avgsen + polpc +
taxpc + region + smsa + wcon +   wtuc + wtrd + wfir + wser + wmfg + wfed + 
wsta + wloc + mix +   county:year + county:avgsen + county:polpc + density:county +  
pctmin:county + county:wcon + county:wtuc + county:wtrd +   county:wfir + county:wmfg + 
county:wsta + county:wloc + pctymle:county +   year:prbconv + year:prbpris + year:polpc + 
year:region +   year:smsa + pctmin:year + year:wtrd + year:wfir + year:wmfg +   year:wsta +
year:mix + pctymle:year + prbarr:prbpris + prbarr:polpc +   density:prbarr + prbarr:region +
pctmin:prbarr + prbarr:wcon +   prbarr:wtuc + prbarr:wtrd + prbarr:wfir + prbarr:wfed +
prbconv:prbpris +   prbconv:polpc + prbconv:smsa + pctmin:prbconv + prbconv:wcon +   
prbconv:wfir + prbconv:wser + prbconv:wmfg + prbconv:mix +   density:prbpris + prbpris:taxpc + 
prbpris:region + pctmin:prbpris +   prbpris:wcon + prbpris:wtrd + prbpris:wmfg + prbpris:wfed + 
prbpris:wsta + prbpris:wloc + density:avgsen + avgsen:taxpc +   avgsen:region + avgsen:smsa + 
pctmin:avgsen + avgsen:wcon +   avgsen:wtrd + avgsen:wfir + avgsen:wser + avgsen:wfed + avgsen:mix +
pctymle:avgsen + polpc:region + polpc:smsa + pctmin:polpc +   polpc:wtuc + polpc:wtrd + polpc:wser + 
polpc:wmfg + polpc:wfed +   density:region + density:smsa + density:pctmin + density:wcon +  density:wtuc +
density:wtrd + density:wsta + density:mix +   density:pctymle + taxpc:region + taxpc:smsa + pctmin:taxpc + 
taxpc:wmfg + taxpc:wfed + taxpc:wsta + taxpc:wloc + region:smsa +   pctmin:region + region:wcon + region:wtuc + 
region:wtrd +   region:wfir + region:wser + region:wmfg + region:wfed + region:wsta +   region:wloc + region:mix + 
pctymle:region + pctmin:smsa +   smsa:wtuc + smsa:wtrd + smsa:wser + smsa:wmfg + smsa:wfed +   smsa:wsta + 
smsa:mix + pctymle:smsa + pctmin:wtrd + pctmin:wfir +   pctmin:wser + pctmin:wmfg + pctmin:wfed + pctmin:wsta + 
pctmin:wloc +   pctmin:mix + pctmin:pctymle + wcon:wtuc + wcon:wfir + wcon:wmfg +   wcon:wfed + wcon:wsta +
wcon:mix + pctymle:wcon + wtuc:wsta +   wtuc:wloc + wtrd:wfir + wtrd:wmfg + wtrd:wsta + wtrd:mix + wfir:wmfg +
wfir:wfed + wfir:wsta + wfir:wloc + wfir:mix +   pctymle:wfir + wser:wmfg + wser:wfed + wser:wloc + wser:mix +
wmfg:wfed + pctymle:wmfg + wfed:wsta + wfed:wloc + wfed:mix +   wsta:wloc + wsta:mix + pctymle:wsta + pctymle:mix"
xs_lasso = model.matrix(as.formula(formula), Crime)
xs_lasso_train = xs_lasso[-test,]
xs_lasso_test = xs_lasso[test,]
grid=10^seq(2,-4,length=100)
lasso.mod=cv.glmnet(xs_lasso[-test ,],ys_train,alpha=1,lambda=grid)


lasso.mod.2=glmnet(xs_lasso[-test ,],ys_train,alpha=1,lambda=grid)
lasso.mod.3=glmnet(xs_lasso[-test ,],ys_train,alpha=1,lambda=lasso.mod$lambda.1se)
ys_lasso_pred = predict(lasso.mod.3, xs_lasso_test, s = lasso.mod$lambda.1se)
lasso_vars =  names(coef(lasso.mod.3)[,1][coef(lasso.mod.3)[,1]!=0])[-1]

lasso_rse = mean(ys_test - ys_lasso_pred)^2
lasso_rse / val.test.errors[which.min(val.test.errors)] * 100
mean(abs((ys_test - ys_lasso_pred)/ ys_test))

######################################################################
# Splines
######################################################################

# To identify outliers
# fit.all <- lm(crmrte~bs(pctmin, knots=pctmin.knots) + log(prbconv) + log(polpc) 
#   + prbarr + density:county + prbarr:prbpris + pctmin:polpc + polpc:wfed + density:pctmin 
#   + density:pctymle + taxpc:wfed + region:wsta, data=Crime)

# k-Folds (k=10)
folds <- createFolds(Crime$pctmin, k=10, list=TRUE, returnTrain=FALSE)

# Split into training/test sets
set.seed(1)
test = sample(nrow(Crime), nrow(Crime)*.15)

# Spline function
#pctmin.knots = attr(bs(Crime$pctmin, df=9), "knots")
pctmin.knots = attr(smooth.spline(Crime$pctmin, cv=TRUE), "knots")

# Sort for plotting later
newdata.pctmin = sort(Crime[test,]$pctmin, index.return=TRUE)
crime.actual = (Crime[test,]$crmrte)[newdata.pctmin$ix]

# Only linear $pctmin
fit.pctmin = lm(crmrte ~ pctmin, data=Crime[-test,])
pred.pctmin = predict(fit.pctmin, newdata=(Crime[test,])[newdata.pctmin$ix,], se.fit=T)
error.pctmin = mean(abs((pred.pctmin$fit - crime.actual)/crime.actual))

# Only poly $pctmin
fit.pctmin.poly = lm(crmrte ~ poly(pctmin,4), data=Crime[-test,])
pred.pctmin.poly = predict(fit.pctmin.poly, newdata=(Crime[test,])[newdata.pctmin$ix,], se.fit=T)
error.pctmin.poly = mean(abs((pred.pctmin.poly$fit - crime.actual)/crime.actual))

# Only splines $pctmin
#pctmin.knots = attr(smooth.spline(Crime$pctmin, cv=TRUE), "knots")
fit.splines = lm(crmrte~bs(pctmin, knots=pctmin.knots), data=Crime[-test,])
pred = predict(fit.splines, newdata=list(pctmin = newdata.pctmin$x), se.fit=T)
error.pctmin.splines = mean(abs((pred$fit - crime.actual)/crime.actual))

# $pctmin spline with $density, prbarr, wfed
fit.splines.2 = lm(crmrte~bs(pctmin, knots=pctmin.knots)+density+prbarr+wfed, data=Crime[-test,])
pred2 = predict(fit.splines.2, newdata=(Crime[test,])[newdata.pctmin$ix,], se.fit=T)
error.pctmin.plus = mean(abs((pred2$fit - crime.actual)/crime.actual))

# $pctmin spline with linear regression predictors **********
fit.splines.3 = lm(crmrte~bs(pctmin, knots=pctmin.knots) + log(prbconv) + log(polpc) + prbarr + density:county + prbarr:prbpris + pctmin:polpc + polpc:wfed + density:pctmin + density:pctymle + taxpc:wfed + region:wsta, data=Crime[-test,])
pred3 = predict(fit.splines.3, newdata=(Crime[test,])[newdata.pctmin$ix,], se.fit=T)
error.pctmin.overall = mean(abs((pred3$fit - crime.actual)/crime.actual))

# $pctmin poly with linear regression predictors (for comp)
fit.poly.4 = lm(crmrte~poly(pctmin,4) + log(prbconv) + log(polpc) + prbarr + density:county + prbarr:prbpris + pctmin:polpc + polpc:wfed + density:pctmin + density:pctymle + taxpc:wfed + region:wsta, data=Crime[-test,])
pred4 = predict(fit.poly.4, newdata=(Crime[test,])[newdata.pctmin$ix,], se.fit=T)
error.pctmin.overall.poly = mean(abs((pred4$fit - crime.actual)/crime.actual))

# k-Folds with smooth splines
kfolds.results = vector()
fit.vector = list()
for (i in 1:length(folds)) {
  fit.splines <- lm(crmrte~bs(pctmin, knots=pctmin.knots) + log(prbconv) + log(polpc) + prbarr + density:county + prbarr:prbpris + pctmin:polpc + polpc:wfed + density:pctmin + density:pctymle + taxpc:wfed + region:wsta, data=Crime[-folds[[i]],])
  fit.vector[[i]] = fit.splines
  pred.spline <- predict(fit.splines, newdata=Crime[folds[[i]],], se.fit=T)
  kfolds.results[i] = mean(abs((pred.spline$fit - Crime[folds[[i]],]$crmrte)/Crime[folds[[i]],]$crmrte))
  #print(paste("Fold", i, "accuracy:", kfolds.results[i]))
}
print(kfolds.results)
print(mean(kfolds.results))

######################################################################
# Plot
######################################################################
plot(Crime$pctmin,Crime$crmrte,col="gray")
lines(newdata.pctmin$x, pred$fit, lwd=2)
lines(newdata.pctmin$x, pred$fit+2*pred$se, lty="dashed")
lines(newdata.pctmin$x, pred$fit-2*pred$se, lty="dashed")

ggplot() + geom_point(aes(x = Crime$pctmin, y = Crime$crmrte), color="darkgrey") + 
  geom_ribbon(aes(x = newdata.pctmin$x, y = pred$fit, ymin= pred$fit-2*pred$se, ymax= pred$fit+2*pred$se), color="lightgrey", alpha =.15)+
  geom_line(aes(x = newdata.pctmin$x, y = pred$fit), color="red")+
  geom_line(aes(x = newdata.pctmin$x, y = pred$fit+2*pred$se), linetype = "dashed") +
  geom_line(aes(x = newdata.pctmin$x, y = pred$fit-2*pred$se), linetype = "dashed") +
  labs(x="Proportion of minority in 1980 (%)", y="Crimes committed per person", title="How is the crime rate related to the proportion of\n    minorities in the region?") +
  theme_fivethirtyeight()

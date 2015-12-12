library(ISLR)
library(leaps)
Crime <- read.csv("Crime.csv")
library(glmnet)

lm.fit = lm(crmrte ~ ., data = Crime)
test = sample(530,100)
test
xs = Crime[-which(names(Crime) %in% c("crmrte"))]
ys = Crime[which(names(Crime) %in% c("crmrte"))]
xs_test = xs[test,]
xs_train = xs[-test,]
ys_test = ys[test,]
ys_train = ys[-test,]
bestsubset=regsubsets(y ~ ., data = data.frame(y = ys_train, x = xs_train), nvmax = 23)

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

lm.2.fit = lm(crmrte ~  prbarr + prbconv + polpc + density + as.factor(region) + pctmin + wfed + pctymle, data = Crime)
summary(lm.2.fit)

lm.3.fit = lm(crmrte ~  log(prbarr) + log(prbconv) + log(polpc) + density + as.factor(region) + pctmin + poly(wfed,3) + pctymle, data = dataset)
summary(lm.3.fit)

lm.4.fit = lm(crmrte ~  .*., data = dataset)
summary(lm.4.fit)

fitnames = c("prbarr" , "prbconv" , "polpc" , "density" , "as.factor(region)" , "pctmin" , "wfed" , "pctymle", "crmrte")
pairs(dataset[names(dataset) %in% fitnames])
anova(lm.2.fit)

lm.5.fit = lm(crmrte ~  log(prbarr) + log(prbconv) + log(polpc) + density + as.factor(region) + pctmin + poly(wfed,3) + pctymle + .*., data = dataset)


library(MASS)
interaction.fit = stepAIC(lm.5.fit)

coefi = coef(interaction.fit)
pred = as.matrix(xs_test[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) 
                                                                %in% x_cols]
val.test.errors = mean((ys_test - pred)^2)
val.test.errors

formula = "~ log(prbconv) + log(polpc) + density + pctmin +   poly(wfed, 3) + pctymle + county + year + prbarr + prbconv +   prbpris + avgsen + polpc + taxpc + region + smsa + wcon +   wtuc + wtrd + wfir + wser + wmfg + wfed + wsta + wloc + mix +   county:year + county:avgsen + county:polpc + density:county +   pctmin:county + county:wcon + county:wtuc + county:wtrd +   county:wfir + county:wmfg + county:wsta + county:wloc + pctymle:county +   year:prbconv + year:prbpris + year:polpc + year:region +   year:smsa + pctmin:year + year:wtrd + year:wfir + year:wmfg +   year:wsta + year:mix + pctymle:year + prbarr:prbpris + prbarr:polpc +   density:prbarr + prbarr:region + pctmin:prbarr + prbarr:wcon +   prbarr:wtuc + prbarr:wtrd + prbarr:wfir + prbarr:wfed + prbconv:prbpris +   prbconv:polpc + prbconv:smsa + pctmin:prbconv + prbconv:wcon +   prbconv:wfir + prbconv:wser + prbconv:wmfg + prbconv:mix +   density:prbpris + prbpris:taxpc + prbpris:region + pctmin:prbpris +   prbpris:wcon + prbpris:wtrd + prbpris:wmfg + prbpris:wfed +   prbpris:wsta + prbpris:wloc + density:avgsen + avgsen:taxpc +   avgsen:region + avgsen:smsa + pctmin:avgsen + avgsen:wcon +   avgsen:wtrd + avgsen:wfir + avgsen:wser + avgsen:wfed + avgsen:mix +   pctymle:avgsen + polpc:region + polpc:smsa + pctmin:polpc +   polpc:wtuc + polpc:wtrd + polpc:wser + polpc:wmfg + polpc:wfed +   density:region + density:smsa + density:pctmin + density:wcon +   density:wtuc + density:wtrd + density:wsta + density:mix +   density:pctymle + taxpc:region + taxpc:smsa + pctmin:taxpc +   taxpc:wmfg + taxpc:wfed + taxpc:wsta + taxpc:wloc + region:smsa +   pctmin:region + region:wcon + region:wtuc + region:wtrd +   region:wfir + region:wser + region:wmfg + region:wfed + region:wsta +   region:wloc + region:mix + pctymle:region + pctmin:smsa +   smsa:wtuc + smsa:wtrd + smsa:wser + smsa:wmfg + smsa:wfed +   smsa:wsta + smsa:mix + pctymle:smsa + pctmin:wtrd + pctmin:wfir +   pctmin:wser + pctmin:wmfg + pctmin:wfed + pctmin:wsta + pctmin:wloc +   pctmin:mix + pctmin:pctymle + wcon:wtuc + wcon:wfir + wcon:wmfg +   wcon:wfed + wcon:wsta + wcon:mix + pctymle:wcon + wtuc:wsta +   wtuc:wloc + wtrd:wfir + wtrd:wmfg + wtrd:wsta + wtrd:mix +   wfir:wmfg + wfir:wfed + wfir:wsta + wfir:wloc + wfir:mix +   pctymle:wfir + wser:wmfg + wser:wfed + wser:wloc + wser:mix +   wmfg:wfed + pctymle:wmfg + wfed:wsta + wfed:wloc + wfed:mix +   wsta:wloc + wsta:mix + pctymle:wsta + pctymle:mix"
xs_lasso = model.matrix(as.formula(formula), dataset)
xs_lasso_train = xs_lasso[-test,]
xs_lasso_test = xs_lasso[test,]
grid=10^seq(2,-4,length=100)
lasso.mod=cv.glmnet(xs_lasso[-test ,],ys_train,alpha=1,lambda=grid)
lasso.mod.2=glmnet(xs_lasso[-test ,],ys_train,alpha=1,lambda=grid)
lasso.mod.3=glmnet(xs_lasso[-test ,],ys_train,alpha=1,lambda=lasso.mod$lambda.1se)
ys_lasso_pred = predict(lasso.mod.3, xs_lasso_test, s = lasso.mod$lambda.1se)

lasso_rse = mean(ys_test - ys_lasso_pred)^2
lasso_rse / val.test.errors[which.min(val.test.errors)] * 100

mean(abs((ys_test - ys_lasso_pred)/ ys_test))




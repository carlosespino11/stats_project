library(ISLR)
library(leaps)
Crime <- read.csv("Crime.csv")

lm.fit = lm(crmrte ~ ., data = Crime)
test = sample(530,100)
test
xs = Crime[-which(names(Crime) %in% c("crmrte"))]
ys = Crime[which(names(Crime) %in% c("crmrte"))]
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

lm.2.fit = lm(crmrte ~  prbarr + prbconv + polpc + density + as.factor(region) + pctmin + wfed + pctymle, data = Crime)
summary(lm.2.fit)


## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(hypr)

## ---- eval = FALSE-------------------------------------------------------
#  vignette("hypr-intro", package = "hypr")

## ------------------------------------------------------------------------
set.seed(123)
M <- c(X1 = 10, X2 = 20, X3 = 10, X4 = 40) # condition means
N <- 5
SD <- 10
simdat <- do.call(rbind, lapply(names(M), function(x) {
  data.frame(X = x, DV = as.numeric(MASS::mvrnorm(N, unname(M[x]), SD^2, empirical = TRUE)))
}))
simdat$id <- 1:nrow(simdat)
simdat

## ------------------------------------------------------------------------
trtC <- hypr(mu1~0, mu2~mu1, mu3~mu1, mu4~mu1)

## ------------------------------------------------------------------------
trtC

## ------------------------------------------------------------------------
contrasts(simdat$X) <- contr.hypothesis(trtC)
contrasts(simdat$X)

## ------------------------------------------------------------------------
round(coef(summary(lm(DV ~ X, data=simdat))), 3)

## ------------------------------------------------------------------------
sumC <- hypr((mu1+mu2+mu3+mu4)/4~0, (3*mu1-mu2-mu3-mu4)/4~0, (3*mu2-mu1-mu3-mu4)/4~0, (3*mu3-mu1-mu2-mu4)/4~0)
sumC

## ------------------------------------------------------------------------
contrasts(simdat$X) <- contr.hypothesis(sumC)
contrasts(simdat$X)

## ------------------------------------------------------------------------
contrasts(simdat$X) <- contr.hypothesis(
  (mu1+mu2+mu3+mu4)/4~0, 
  (3*mu1-mu2-mu3-mu4)/4~0, 
  (3*mu2-mu1-mu3-mu4)/4~0, 
  (3*mu3-mu1-mu2-mu4)/4~0
)
contrasts(simdat$X)

## ------------------------------------------------------------------------
round(coef(summary(lm(DV ~ X, data=simdat))),3)


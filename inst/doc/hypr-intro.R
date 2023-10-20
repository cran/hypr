## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(hypr)

## -----------------------------------------------------------------------------
trtC <- hypr(mu1~0, mu2~mu1, mu3~mu1, mu4~mu1)

## -----------------------------------------------------------------------------
trtC <- hypr(base = mu0~0, trt1 = mu1~mu0, trt2 = mu2~mu0, trt3 = mu3~mu0)

## -----------------------------------------------------------------------------
trtC

## -----------------------------------------------------------------------------
hypr(one~0, two~one, three~one, four~one, levels = c("one", "two", "three", "four"))

## -----------------------------------------------------------------------------
hypr(one~0, two~one, three~one, four~one, levels = c("one", "two", "three", "four", "five"))

## -----------------------------------------------------------------------------
formula(trtC) # a list of equations
levels(trtC) # a vector of corresponding factor levels (variables in equations)
names(trtC) # a vector of corresponding contrast names
hmat(trtC) # the hypothesis matrix
thmat(trtC) # the transposed hypothesis matrix (as displayed in the summary)
cmat(trtC) # the contrast matrix

## -----------------------------------------------------------------------------
otherC <- hypr()
cmat(otherC) <- cbind(int = 1, contr.treatment(4)) # add intercept to treatment contrast
otherC

## -----------------------------------------------------------------------------
cmat(trtC)

## -----------------------------------------------------------------------------
cmat(trtC, remove_intercept = TRUE)

## -----------------------------------------------------------------------------
helC <- hypr(m2~m1, m3~(m1+m2)/2, m4~(m1+m2+m3)/3)
cmat(helC)

## ----eval=FALSE---------------------------------------------------------------
#  cmat(helC, remove_intercept = TRUE) # throws an error

## -----------------------------------------------------------------------------
contr.hypothesis(trtC) # removes `base` column
contr.hypothesis(helC) # removes nothing

## -----------------------------------------------------------------------------
contr.hypothesis(m1~0, m2~m1, m3~m1)
contr.hypothesis(m2~m1, m3~(m1+m2)/2, m4~(m1+m2+m3)/3)


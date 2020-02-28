## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(hypr)

## ---- eval = FALSE------------------------------------------------------------
#  vignette("hypr-intro", package = "hypr")

## ---- eval = FALSE------------------------------------------------------------
#  vignette("hypr-regression", package = "hypr")

## -----------------------------------------------------------------------------
contr.treatment(4) # a contrast matrix for one baseline and 3 treatments

## -----------------------------------------------------------------------------
h <- hypr()
cmat(h) <- contr.treatment(4)
h

## -----------------------------------------------------------------------------
cmat(h, add_intercept = TRUE) <- contr.treatment(4)
h


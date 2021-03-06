% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hypr.R
\name{hypr}
\alias{hypr}
\title{Create a hypr object}
\usage{
hypr(..., levels = NULL, order_levels = missing(levels))
}
\arguments{
\item{...}{A list of null hypothesis equations}

\item{levels}{(Optional) A list of terms/levels to use. If supplied, matrix rows/columns will be in this order. An error will be thrown if an equation contains a level that is not in this vector.}

\item{order_levels}{(Optional) Whether to order the rows/columns of the hypothesis/contrast matrices alphabetically. Default is \code{TRUE} if \code{levels} were not explicitly provided.}
}
\value{
A \code{hypr} object
}
\description{
Use this function to create hypr objects from null hypothesis equations. Each argument should be one equation. For example, a null hypothesis for the grand mean (GM), often used as the intercept, is usually coded as \code{mu~0}.
}
\details{
You may call the function without any arguments. In that case, an empty hypr object is returned. This is useful if you want to derive equations from a known hypothesis matrix or contrast matrix.

Basic specification of contrasts in R is accomplished with basic R functions \code{\link[stats:contrasts]{stats::contrasts()}} and \code{\link[stats:C]{stats::C()}} (Chambers & Hastie, 1992). Other relevant packages for this topic are \code{multcomp} (Bretz et al., 2010), \code{contrast} (Kuhn et al., 2016), and, including also various vignettes, \code{emmeans} (Lenth, 2019).
}
\examples{

# Create an empty hypr object (no hypotheses):
h <- hypr()

# Treatment contrast:
h <- hypr(mu1~0, mu2~mu1, mu3~mu1, mu4~mu1)

# Identical version:
h <- hypr(~mu1, ~mu2-mu1, ~mu3-mu1, ~mu4-mu1)

contr.hypothesis(h)


# Generate a dataset
set.seed(123)
M <- c(mu1 = 10, mu2 = 20, mu3 = 10, mu4 = 40) # condition means
N <- 5 # number of observations per condition
SD <- 10 # residual SD
simdat <- do.call(rbind, lapply(names(M), function(x) {
  data.frame(X = x, DV = as.numeric(MASS::mvrnorm(N, unname(M[x]), SD^2, empirical = TRUE)))
}))
simdat$X <- factor(simdat$X, levels=levels(h))
simdat

# Check agreement of hypothesis levels and factor levels
stopifnot(levels(h) == levels(simdat$X))

# Linear regression
contrasts(simdat$X) <- contr.hypothesis(h)

round(coef(summary(lm(DV ~ X, data=simdat))),3)

}
\references{
Chambers, J. M. and Hastie, T. J. (1992) \emph{Statistical models}. Chapter 2 of \emph{Statistical Models} in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.

Frank Bretz, Torsten Hothorn and Peter Westfall (2010), \emph{Multiple Comparisons Using R}, CRC Press, Boca Raton.

Max Kuhn, contributions from Steve Weston, Jed Wing, James Forester and Thorn Thaler (2016). \emph{contrast: A Collection of Contrast Methods}. R package version 0.21. \url{https://CRAN.R-project.org/package=contrast}

Lenth, R. (2019). \emph{emmeans: Estimated Marginal Means, aka Least-Squares Means}. R package version 1.4.1. \url{https://CRAN.R-project.org/package=emmeans}
}
\seealso{
\code{\link[stats]{contrasts}} and \code{\link[stats]{C}} for basic specification of contrasts in R, S4 class \code{\link[hypr:hypr-class]{hypr}}, \code{\link[hypr]{cmat}}, \code{\link[hypr]{contr.hypothesis}} for retrieval of contrast matrices from \code{hypr} objects
}

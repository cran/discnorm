## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(discnorm )

## ------------------------------------------------------------------------
#let us discretize an underlying normal vector
# with moderate correlation 
rho <- 0.3
Sigma <- diag(5)
Sigma[Sigma !=1] <- rho
set.seed(1234)
norm.sample  <- MASS::mvrnorm(n=200, mu=rep(0,5), Sigma=Sigma)
# let us discretize into 4 categories
disc.sample <- apply(norm.sample, 2, cut,   breaks=c(-Inf, -1, 1, 2, Inf), labels=FALSE)

#check for underlying normality
pvalue <- bootTest(disc.sample, B=500)
print(pvalue)
# we have no evidence against the null hypothesis of underlying normality

## ------------------------------------------------------------------------
nonnorm.sample <- data.frame(norm.sample[, 1:4], norm.sample[,1]*norm.sample[,2])
disc.sample2 <- apply(nonnorm.sample, 2, cut, breaks=c(-Inf, -1, 1, 2, Inf), labels=FALSE)
pvalue <- bootTest(disc.sample2, B=500)
print(pvalue)
# rejected!


library(gratia)
library(mgcv)
set.seed(20)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)

mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
summary(mod)
plot.gam(mod)
vis.gam(mod)

draw(mod, ncol = 2, residuals = TRUE)   # Plots
 
appraise(mod) # diagnostic plots

qq_plot(mod)


## simulate example... from ?mgcv::factor.smooth.interaction
set.seed(0)
## simulate data...
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x, a=2, b=-1) exp(a * x)+b
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
n <- 500
nf <- 10
fac <- sample(1:nf, n, replace=TRUE)
x0 <- runif(n)
x1 <- runif(n)
x2 <- runif(n)
a <- rnorm(nf) * .2 + 2;
b <- rnorm(nf) * .5
f <- f0(x0) + f1(x1, a[fac], b[fac]) + f2(x2)
fac <- factor(fac)
y <- f + rnorm(n) * 2

df <- data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, fac = fac)
mod <- gam(y~s(x0) + s(x1, fac, bs="fs", k=5) + s(x2, k=20),
           method = "ML")
draw(mod)
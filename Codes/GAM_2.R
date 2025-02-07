require(mgcv)

mcycle <- MASS::mcycle

# Examine the mcycle data frame
head(mcycle)
plot(mcycle)

# Fit a linear model
lm_mod <- lm(accel~times, data = mcycle)

# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)
 

gam_mod <- gam(accel ~ s(times), data = mcycle)

# Plot the results
plot(gam_mod, residuals = TRUE, pch = 1)
plot(gam_mod,residuals = TRUE, pch= 3) # pch just changes the shape of the residual vizuals

coef(gam_mod)

# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)

# Fit with 20 basis functions
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

# Visualize the GAMs
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)



# The smoothing parameter balances between likelihood and wiggliness
# to optimize model fit.

# Extract the smoothing parameter
gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
gam_mod$sp

# Fix the smoothing parameter at 0.1
gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)

# Fix the smoothing parameter at 0.0001
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)

# Plot both models
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 3)
plot(gam_mod_s2, residuals = TRUE, pch = 3)
 
library(gamair)

data("mpg", package="gamair")


# Examine the data
head(mpg)
str(mpg)

# Fit the model
mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price), 
                data = mpg, method = "REML")

# Plot the model
plot(mod_city, pages = 1)


# Including categorical variables
mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + 
                   fuel + drive + style,
                 data = mpg, method = "REML")
# Plot the model
plot(mod_city2, all.terms = TRUE, pages = 1)

mod_city3 <- gam(city.mpg ~ s(weight, by = drive) + 
                   s(length, by = drive) +
                   s(price, by = drive) + drive,
                 data = mpg, method = "REML")
plot(mod_city3, pages = 1)

summary(mod_city3)

plot(mod_city3, pages = 1)

mod_city4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                 data = mpg, method = "REML")

summary(mod_city4)
plot(mod_city4, page =1)

###############################################################################

#       VIsualizations
mod <- gam(accel ~ s(times), data = mcycle, method = "REML")

plot(mod, residuals = TRUE,pch =3, 
     shade = TRUE, shade.col = "red" )

mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")
summary(mod)

plot(mod, select = 3)
plot(mod, pages = 1, all.terms = TRUE)

# Plot the weight effect
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink")

# Make another plot adding the intercept value and uncertainty
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1], seWithMean = TRUE)


# Model diagnostics using gam.check()

set.seed(0)
dat <- gamSim(1,n=200)

mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")
gam.check(mod) 
        #x2 does not have enough basis functions because it has a significant result 
        # in the diagnostic test.


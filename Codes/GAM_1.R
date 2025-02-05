
library(mgcv)
library(itsadug)
library(tidyverse)

isit <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Additve_modelling\\Data\\ISIT.csv")
view(isit)

# Focus on season 2

isit2 <- subset(isit, Season == 2)

linear_model <- gam(Sources ~ SampleDepth, data = isit2)
summary(linear_model)

linear_model_lm <- lm(Sources ~ SampleDepth, data = isit2)
summary(linear_model_lm)   # This is similar to the GAM function output

m1 <-ggplot(data = isit2, aes(y = Sources, x = SampleDepth)) +
  geom_point() + 
  geom_line(aes(y = fitted(linear_model)), # so cool!!
            colour = "red",
           linewidth = 1.2) +
  theme_bw()


# Now lets use a GAM
gam_model <- gam(Sources ~ s(SampleDepth), data = isit2)
summary(gam_model)


m1+geom_line(aes(y = fitted(gam_model)), # so cool!!
             colour = "blue",
             linewidth = 1.2) 

plot(gam_model)   # Useful to look at the smooths

# test whether the non-linear model offers a significant improvement over the linear model
linear_model <- gam(Sources ~ SampleDepth, data = isit2)
smooth_model <- gam(Sources ~ s(SampleDepth), data = isit2)
AIC(linear_model, smooth_model)


# GAM with linear and smooth terms

isit$Season <- as.factor(isit$Season)

basic_model <- gam(Sources ~ Season + s(SampleDepth), data = isit,
                   method = "REML")
basic_summary <- summary(basic_model)

basic_summary$p.table # The linear effect
basic_summary$s.table #The non-linear effect


par(mfrow = c(1, 2))
plot(basic_model, all.terms = TRUE)

linear_model2 <- gam(Sources ~ SampleDepth, data = isit)

m3<-ggplot(data = isit, aes(y = Sources, x = SampleDepth)) +
  geom_point() + 
  geom_line(aes(y = fitted(linear_model2)),  
            colour = "red",
            linewidth = 1.2) +
  theme_bw()

m3+
geom_line(data=isit, aes(y = fitted(basic_model)),  
             colour = "green",
             linewidth = 1, alpha= 0.5) 

# Because the number of free parameters in GAMs is difficult to define,
#the EDF are instead related to the smoothing parameter λ
# the greater the penalty, the smaller the EDF.




# GAM with multiple linear and smooth terms
two_term_model <- gam(Sources ~ Season + s(SampleDepth) + RelativeDepth,
                      data = isit, method = "REML")
two_term_summary <- summary(two_term_model)
two_term_summary$p.table
two_term_summary$s.table

par(mfrow = c(2, 2))
plot(two_term_model, all.terms = TRUE)





#  GAM with multiple smooth terms

two_smooth_model <- gam(Sources ~ Season + s(SampleDepth) + s(RelativeDepth),
                        data = isit, method = "REML")
two_smooth_summary <- summary(two_smooth_model)

two_smooth_summary$p.table
two_smooth_summary$s.table   # My guess that the reason there is no Estimate is that
                              # it does not make any sense for non-linear relationships

par(mfrow = c(2, 2))
plot(two_smooth_model, page = 1, all.terms = TRUE)


AIC(basic_model, two_term_model, two_smooth_model)  # Model comparision


# GAMS with interaction terms

factor_interact <- gam(Sources ~ Season + 
                         s(SampleDepth, by = Season) + # Use "by" since Season is categorical
                         s(RelativeDepth), data = isit, method = "REML")
summary(factor_interact)$s.table

par(mfrow = c(2, 2))
plot(factor_interact, all.terms = TRUE)
 
AIC(two_smooth_model, factor_interact)

# Two smoothed variables interaction
smooth_interact <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth),
                       data = isit, method = "REML")
summary(smooth_interact)$s.table


m3+
  geom_line(aes(y=fitted(smooth_interact)),
            colour = "blue",
            linewidth = 1, alpha= 0.5)

plot(smooth_interact, page = 1, scheme = 2)
 


# GM Validation

smooth_interact <- gam(Sources ~ Season + s(SampleDepth, RelativeDepth),
                       data = isit, method = "REML")
summary(smooth_interact)$p.table
summary(smooth_interact)$s.table

k.check(smooth_interact) #EDF are very close to k
      # This means the wiggliness of the model is being overly constrained by the default k
smooth_interact_k60 <- gam(Sources ~ Season +
                             s(SampleDepth, RelativeDepth, k = 60), 
                           data = isit, method = "REML")

k.check(smooth_interact_k60)


par(mfrow = c(2, 2))
gam.check(smooth_interact_k60)

`?`(family.mgcv)


# GAM WITH TWIDDIE FAMILY

# Hint!  Because the Normal distribution is the default
# setting, we have not specified the distribution in this
# workshop yet.
# Here is how we would write the model to specify the
# Normal distribution:


smooth_interact <- gam(Sources ~ Season + 
                         s(SampleDepth, RelativeDepth, k = 60),
                       family = gaussian(link = "identity"), 
                       data = isit,
                       method = "REML")


summary(smooth_interact_tw)$p.table
gam.check(smooth_interact_tw)
gam.check(smooth_interact)

AIC(smooth_interact, smooth_interact_tw)



#   CYLINDRICAL DATA

# Nottingham temperature time series
data(nottem)
view(nottem)
# the number of years of data (20 years)
n_years <- length(nottem)/12

# categorical variable coding for the 12 months of the
# year, for every year sampled (so, a sequence 1 to 12
# repeated for 20 years).
nottem_month <- rep(1:12, times = n_years)


# the year corresponding to each month in nottem_month
nottem_year <- rep(1920:(1920 + n_years - 1), each = 12)

nottem_combined <- as.data.frame(cbind(nottem_month, nottem_year, nottem))
# Plot the time series
 
ggplot(aes(x = nottem_month, y = nottem, colour = factor(nottem_year)), 
       data= nottem_combined)+
  geom_line()+
  theme_bw()

year_gam <- gam(nottem ~ s(nottem_year) +
                  s(nottem_month, bs = "cc"), # cyclical cubic spline
                method = "REML")
summary(year_gam)$s.table

plot(year_gam, page = 1, scale = 0)

plot(year_gam, pages = 1, scale = 0)
gam.check(year_gam)

par(mfrow = c(1, 2))
acf(resid(year_gam), lag.max = 36, main = "ACF")
pacf(resid(year_gam), lag.max = 36, main = "pACF")

# GAMM


df <- data.frame(nottem, nottem_year, nottem_month)

year_gam <- gamm(nottem ~ s(nottem_year) + 
                   s(nottem_month, bs = "cc"),
                 data = df)

year_gam_AR1 <- gamm(nottem ~ s(nottem_year) + 
                       s(nottem_month, bs = "cc"),
                     correlation = corARMA(form = ~1 | nottem_year, p = 1),
                     data = df)

year_gam_AR2 <- gamm(nottem ~ s(nottem_year) + 
                       s(nottem_month, bs = "cc"),
                     correlation = corARMA(form = ~1 | nottem_year, p = 2), 
                     data = df)

AIC(year_gam$lme, year_gam_AR1$lme, year_gam_AR2$lme)

#   GAMM with random intercercept

# generate and view data
gam_data2 <- gamSim(eg = 6)  # gamSim [mgcv] simulate example data for GAMs
head(gam_data2)

# run random intercept model
gamm_intercept <- gam(y ~ s(x0) + s(fac, bs = "re"), 
                      data = gam_data2,
                      method = "REML")  # The random effect is 'fac', specified

# examine model output
summary(gamm_intercept)$s.table
plot(gamm_intercept, select =2)
# select = 2 because the random effect appears as the
# second entry in the summary table.

# ploting all levels of the random effect
par(mfrow = c(1, 2), cex = 1.1)

# Plot the summed effect of x0 (without random effects)
plot_smooth(gamm_intercept, view = "x0", # X0 is a main effect
            rm.ranef = TRUE, # Remove random effect
            main = "intercept + s(x1)") # Title

# Plot each level of the random effect
plot_smooth(gamm_intercept, view = "x0", rm.ranef = FALSE, cond = list(fac = "1"),
            main = "... + s(fac)", col = "orange", ylim = c(0, 25))
plot_smooth(gamm_intercept, view = "x0", rm.ranef = FALSE, cond = list(fac = "2"),
            add = TRUE, col = "red")
plot_smooth(gamm_intercept, view = "x0", rm.ranef = FALSE, cond = list(fac = "3"),
            add = TRUE, col = "purple")
plot_smooth(gamm_intercept, view = "x0", rm.ranef = FALSE, cond = list(fac = "4"),
            add = TRUE, col = "turquoise")

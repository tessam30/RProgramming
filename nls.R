# --- Shrinkage estimators

# -- "::" - lets you access functions in a package that haven't been loaded'

# --- Jitter binaries using
# geom_jitter(position = position_jitter(height=0.2))

# pull all columns except one with "name"
# wineTrain <- wine[, which(names(wine) != "Culbivar")]


# "www.jaredlander.com/data/ideo.rdata"
load("ideo.rdata")
head(ideo)

# -- plot a separate model for each year
theYears <- unique(ideo$Year)

# Create an empty list with placeholers
results <- vector(mode ="list", length = length(theYears))

names(results) <- theYears

for(i in theYears)
{
    results[[as.character(i)]] <- glm(Vote ~ Race + Income + Education,
                                      data = ideo, subset=Year==i,
                                      family = binomial(link = "logit"))
}

library(coefplot)

voteInfo <- multiplot(results, coefficients = "Raceblack", plot=FALSE)

head(voteInfo)

# Plot all coefficients for black race over time
multiplot(results, coefficients = "Raceblack", secret.weapon=TRUE) +
    coord_flip(xlim = c(-20, 10))

# Something wrong with 1984
# Use an informative prior to fix problem
resultsB <- vector(mode="list", length = length(theYears))
names(resultsB) <- theYears

for(i in theYears)
{
  resultsB[[as.character(i)]] <- arm::bayesglm(Vote ~ Race + Income + Education,
                                               data = ideo[ideo$Year == i, ],
                                               family = binomial(link = "logit"),
                                               prior.scale = 2.5,
                                               prior.df = 1)
}

multiplot(resultsB, coefficients = "Raceblack", secret.weapon = TRUE)

# --- nonlinear least squares
load("wifi.rdata")
head(wifi)

library(ggplot2)

ggplot(wifi, aes(x=x, y=y, color = Distance)) + geom_point() +
  scale_color_gradient2(low ="blue", mid = "white", high ="red",
                        midpoint = mean(wifi$Distance))

# Distance formuala is not linear so estimate using nonlinear regression
# dist = sqrt()
wifiMod1 <- nls(Distance ~ sqrt((betaX - x)^2 + (betaY - y)^2), data = wifi,
                start= list(betaX = 50, betaY = 50))
summary(wifiMod1)

ggplot(wifi, aes(x=x, y=y, color = Distance)) + geom_point() +
  scale_color_gradient2(low ="blue", mid = "white", high ="red",
                        midpoint = mean(wifi$Distance)) +
  geom_point(data = as.data.frame(t(coef(wifiMod1))), aes(x=betaX, y = betaY), 
             size = 5, color = "green" )

# Fitting splines - best way to fit line to data using squiggliness
data("diamonds")
diaSpline1 <- smooth.spline(x = diamonds$carat, y = diamonds$price)
diaSpline1 <- smooth.spline(x = diamonds$carat, y = diamonds$price, df = 10)
diaSpline1 <- smooth.spline(x = diamonds$carat, y = diamonds$price, df = 100)

# Build helper function
get.spline.info <- function(object)
{
  data.frame(x = object$x, y = object$y, df = object$df)
}

library(dplyr)

spline

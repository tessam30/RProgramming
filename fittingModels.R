# --- Modeling poisson processes
setwd("C:/Users/t/Documents/R/")

# Load packages
library(ggplot2)
library(useful)
library(coefplot)

acs <- read.table("acs_ny.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(acs)

# Create new var
acs$Income <- with(acs, FamilyIncome >= 150000)

# Check distribution of incomes
ggplot(acs, aes(x = FamilyIncome)) + geom_density(fill = "grey", color = "grey") +
  geom_vline(xintercept = 150000) +
  scale_x_continuous(label = multiple.dollar, limits=c(0,1000000))

# Fit logistic
income1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent +
                 NumBedrooms + FamilyType, 
               data = acs, family = binomial(link = "logit"))

# Look at summary
summary(income1)
coefplot(income1)

income1$coefficients

head(fortify(income1))



#Convert to standard scale
invlogit <- function(x) {
  1/(1 + exp(-x))
}

coef <- invlogit(income1$coefficients)

# ---- Fit poisson regression
ggplot(acs, aes(x = NumChildren)) + geom_histogram(binwidth = 1)

children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data = acs, family = poisson(link = "log"))


summary(children1)
coefplot(children1) # Check results with graphics

# Check for overdispersion
z <- (acs$NumChildren - children1$fitted.values)/
  sqrt(children1$fitted.values)

# Sum of overdispersion / degrees of freedom (great than 2 overdisperson)
sum(z^2) / children1$df.residual
# 1.46 is not quite overdispersed; check another test (Chi d

# P-value indicates that there is overdispersion
pchisq(sum(z^2), df = children1$df.residual)

# Account for this
children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data = acs, family = quasipoisson(link = "log"))

#Quasi-poisson uses negative binomial
multiplot(children1, children2)

# Negative binomial inflates standard errors a tad to account for overdispersion

# --------------- GLM NET 
library(glmnet)

# Have to feed it a predictor and response matrix
acsX <- build.x(Income ~ NumBedrooms + NumRooms + NumPeople +
                  NumRooms + NumUnits + NumVehicles + NumWorkers +
                  OwnRent  + YearBuilt + ElectricBill + FoodStamp +
                  HeatingFuel + Insurance + Language -1 ,
                data = acs, contrasts = FALSE)










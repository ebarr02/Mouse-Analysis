# Data

mouse <- read.csv("C:/Users/thegr/OneDrive - UBC/UBC Sauder Classes/Sauder Year 6 (2026-27)/2026W1/STAT 404/Assignments/Project/6. Project Data.csv", sep = ",", header = TRUE)

## Weight (light = -1, heavy = 1)
## Shape (ambidextrous = -1, ergonomic = 1)
## Size (small = -1, large = 1)
## Connection (wireless = -1, wired = 1)
## Person (Jay = -1, Evan = 1)
## PC Setup (Evan's PC = -1, Jay's PC = 1)

# Load libraries

library (MASS)

# Main effects plot

mouse$weight <- as.factor(mouse$weight)
mouse$shape <- as.factor(mouse$shape)
mouse$size <- as.factor(mouse$size)
mouse$connection <- as.factor(mouse$connection)
mouse$person <- as.factor(mouse$person)
mouse$pc <- as.factor(mouse$pc)

plot.design(tracking_accuracy ~ weight + shape + size
            + connection + person + pc, data = mouse)

## Interaction plots

interaction.plot(mouse$weight, mouse$shape, mouse$tracking_accuracy)
interaction.plot(mouse$weight, mouse$size, mouse$tracking_accuracy)
interaction.plot(mouse$weight, mouse$connection, mouse$tracking_accuracy)
interaction.plot(mouse$weight, mouse$person, mouse$tracking_accuracy)
interaction.plot(mouse$weight, mouse$pc, mouse$tracking_accuracy) ### some interaction

interaction.plot(mouse$shape, mouse$size, mouse$tracking_accuracy) ### some interaction **
interaction.plot(mouse$shape, mouse$connection, mouse$tracking_accuracy) ### some interaction **
interaction.plot(mouse$shape, mouse$person, mouse$tracking_accuracy)
interaction.plot(mouse$shape, mouse$pc, mouse$tracking_accuracy) ### some interaction

interaction.plot(mouse$size, mouse$connection, mouse$tracking_accuracy)
interaction.plot(mouse$size, mouse$person, mouse$tracking_accuracy)
interaction.plot(mouse$size, mouse$pc, mouse$tracking_accuracy) ### some interaction

interaction.plot(mouse$connection, mouse$person, mouse$tracking_accuracy)
interaction.plot(mouse$connection, mouse$pc, mouse$tracking_accuracy) ### some interaction

interaction.plot(mouse$person, mouse$pc, mouse$tracking_accuracy)

# Model 1: Main-effects model

mouse.aov <- aov(tracking_accuracy ~ weight + shape + size + connection + person + pc, data = mouse)
summary(mouse.aov)

# Model diagnostics

## Box-cox diagnostic (no transformation needed)

boxcox(tracking_accuracy ~ weight + shape + size
       + connection + person + pc, data = mouse,
       lambda = seq(0, 3, len = 30), ylab = "Log likelihood")

## Q-Q plot of residuals
qqnorm(mouse.aov$residuals, xlab = "Quantile of standard normal", 
       ylab = "Ordered residual")
qqline(mouse.aov$residuals, col = "red", lwd = 2)

## Residuals versus fitted values
plot(mouse.aov$fitted.values, mouse.aov$residuals)

# Model 2: Fit in interaction terms we think will be significant based on interaction plots

mouse2.aov <- aov(tracking_accuracy ~ weight + shape + size + connection + person + pc 
                  + shape:size + shape:connection + weight:pc + shape:pc+ size:pc, + connection:pc, data = mouse)
summary(mouse2.aov)

# Model 3: Reduced model with only significant effects

mouse3.aov <- aov(tracking_accuracy ~ weight + shape + size + connection + person + shape:size, data = mouse)
summary(mouse3.aov)

# Residual diagnostics

## Box-cox diagnostic (no transformation needed)

boxcox(tracking_accuracy ~ weight + shape + size
       + connection + person + pc + shape:size, data = mouse,
       lambda = seq(0, 3, len = 30), ylab = "Log likelihood")

## Q-Q plot of residuals
qqnorm(mouse3.aov$residuals, xlab = "Quantile of standard normal", 
       ylab = "Ordered residual")
qqline(mouse3.aov$residuals, col = "red", lwd = 2)

## Residuals versus fitted values
plot(mouse3.aov$fitted.values, mouse.aov$residuals)

### Comment: Work with the model 3 from here onwards

# Main effect contrasts / Interaction contrasts



# Confidence intervals for contrast effects



# Logistic regression model

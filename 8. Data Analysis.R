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

## Interaction plots

plot.design(tracking_accuracy ~ weight + shape + size
            + connection + person + pc, data = mouse)

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

# Box-cox diagnostic (no transformation needed)

boxcox(tracking_accuracy ~ weight + shape + size
       + connection + person + pc, data = mouse,
       lambda = seq(0, 3, len = 30), ylab = "Log likelihood")

# Main-effects model

mouse.aov <- aov(tracking_accuracy ~ weight + shape + size + connection + person + pc, data = mouse)
summary(mouse.aov)

# Other residual diagnostics

## Q-Q plot of residuals
qqnorm(mouse.aov$residuals, xlab = "Quantile of standard normal", 
       ylab = "Ordered residual")

## Residuals versus fitted values
plot(mouse.aov$fitted.values, mouse.aov$residuals)

# Fit in 2 interaction terms with shape a based on interaction plots

mouse2.aov <- aov(tracking_accuracy ~ weight + shape + size + connection + person + pc + shape:size + shape:connection, data = mouse)
summary(mouse2.aov)

# Interaction contrasts



# Logistics regression model

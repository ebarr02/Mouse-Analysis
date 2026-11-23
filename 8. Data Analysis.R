# Data

library(tidyverse)
library(readxl)

path <- file.choose()          # a window opens: select mouse_analysis_data.xlsx
setwd(dirname(path))           # sets the working directory to that file's folder
getwd()                        # confirms it

mouse <- as.data.frame(read_excel(path))
names(mouse) <- c("mouse", "weight", "size", "shape", "connection", "person", "pc",
                  "score_tracking", "tracking_accuracy", "flicking")

# The -1 values are stored as text with a special minus sign; convert to numbers
fix <- function(x) as.numeric(gsub("\u2212", "-", as.character(x)))
for (v in c("weight", "size", "shape", "connection", "person", "pc")) {
  mouse[[v]] <- fix(mouse[[v]])
}

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
                  + shape:size + shape:connection + weight:pc + shape:pc+ size:pc + connection:pc, data = mouse)
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
#to_num <- function(x) as.numeric(gsub("\u2212", "-", trimws(as.character(x))))
#TukeyHSD(mouse3.aov, which = c("weight", "size", "connection", "person", "shape:size"))
#w <- to_num(mouse$weight)
#h <- to_num(mouse$shape)
#s <- to_num(mouse$size)
#c_conn <- to_num(mouse$connection)
#p <- to_num(mouse$person)

#mouse3.num <- lm(tracking_accuracy ~ w + h + s + c_conn + p + h:s, data = mouse)

#contrasts_est <- 2 * coef(mouse3.num)[-1]
#round(contrasts_est, 3)


# Confidence intervals for contrast effects
## 1. Unadjusted 95% t-confidence intervals for all main & interaction contrasts
#ci_95 <- 2 * confint(mouse3.num, level = 0.95)[-1, ]
#round(cbind(Contrast = contrasts_est, ci_95), 3)

## 2. Family-wise adjusted 95% CIs for pairwise shape & size combinations
#TukeyHSD(mouse3.aov, which = "shape:size", conf.level = 0.95)

# Main effect & interaction contrasts + 95% Confidence Intervals
w      <- as.numeric(as.character(mouse$weight))
h      <- as.numeric(as.character(mouse$shape))
s      <- as.numeric(as.character(mouse$size))
c_conn <- as.numeric(as.character(mouse$connection))
p      <- as.numeric(as.character(mouse$person))

mouse3.num <- lm(tracking_accuracy ~ w + h + s + c_conn + p + h:s, data = mouse)

# Contrasts & unadjusted 95% CIs
round(cbind(Contrast = 2 * coef(mouse3.num)[-1], 2 * confint(mouse3.num)[-1, ]), 3)

# Forest plot of mouse treatment contrasts & 95% CIs
ci_df <- data.frame(
  Term     = c("Weight (Heavy - Light)", "Shape (Ergo - Ambi)",
               "Size (Large - Small)", "Connection (Wired - Wireless)",
               "Person (Evan - Jay)", "Shape x Size"),
  Contrast = 2 * coef(mouse3.num)[-1],
  Lower    = 2 * confint(mouse3.num)[-1, 1],
  Upper    = 2 * confint(mouse3.num)[-1, 2]
)
ci_df$Significant <- (ci_df$Lower > 0) | (ci_df$Upper < 0)
ci_mouse <- subset(ci_df, Term != "Person (Evan - Jay)")

ggplot(ci_mouse, aes(x = Contrast, y = reorder(Term, Contrast), color = Significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_linerange(aes(xmin = Lower, xmax = Upper), linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#C84B31")) +
  labs(
    title = "95% Confidence Intervals for Mouse Treatment Contrasts",
    x = "Contrast Estimate (Tracking Accuracy %)",
    y = NULL
  ) +
  theme_minimal()

# Pairwise shape x size comparisons (table + Tukey 95% CI plot)
tuk <- TukeyHSD(mouse3.aov, which = "shape:size", conf.level = 0.95)
tuk
par(mar = c(5, 8, 4, 2))
plot(tuk, las = 1)


# Logistic regression model

## Response as a proportion (accuracy is a percentage)
mouse$acc_prop <- mouse$tracking_accuracy / 100

## Fit (same terms as Model 3)
logit.fit <- glm(acc_prop ~ weight + shape + size + connection + person + shape:size,
                 data = mouse, family = quasibinomial)

## Results
summary(logit.fit)
anova(logit.fit, test = "F")

## Odds ratios with 95% confidence intervals
round(exp(cbind(OR = coef(logit.fit), confint.default(logit.fit))), 3)

## Fitted accuracy for each shape x size combination
aggregate(fitted(logit.fit) ~ shape + size, data = mouse, FUN = mean)

## Diagnostics
plot(fitted(logit.fit), residuals(logit.fit, type = "pearson"),
     xlab = "Fitted proportion", ylab = "Pearson residual")
abline(h = 0, lty = 2)
qqnorm(residuals(logit.fit, type = "deviance"),
       xlab = "Quantile of standard normal", ylab = "Ordered deviance residual")

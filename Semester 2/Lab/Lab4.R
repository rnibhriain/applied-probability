# Read the data into R
exp <- read.csv(file='Lab4_ExperimentData.csv')
# View the first few lines of the data
head(exp)

# Scatterplot
plot(exp$Question, exp$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
# Fit and summarise the slr model fit to the data
lm1 <- lm(Seconds ~ Question, data = exp)
summary(lm1)

# Scatterplot with lm fit
plot(exp$Question, exp$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
abline(lm1)

# Store the residuals, standardised residuals and predicted values
resids <- resid(lm1)
sresids = rstandard(lm1)
preds <- predict(lm1)
# Plot the residuals
par(mfrow = c(1, 2), mar = c(5,6,2,2))
plot(preds, resids, xlab = "Predicted", ylab = "Residuals",
     main = "Residuals versus predicted")
abline(h = 0)
# QQ probability plot
qqnorm(resids, ylab="Residuals", xlab="Normal Scores",
       main="QQ plot")
qqline(resids)

par(mfrow = c(1,1), mar = c(5,4,4,2))

# Create a subset of the original dataset
exp_subset <- exp[which(exp$Question <5 & exp$Seconds < 98),]
summary(exp_subset)

# Scatterplot
plot(exp_subset$Question, exp_subset$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
# Fit and summarise the slr model fit to the data
lm2 <- lm(Seconds ~ Question, data = exp_subset)
summary(lm2)

# Scatterplot with lm fit
plot(exp_subset$Question, exp_subset$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
abline(lm2)


# Store the residuals, standardised residuals and predicted values
resids <- resid(lm2)
sresids = rstandard(lm2)
preds <- predict(lm2)
# Plot the residuals
par(mfrow = c(1, 2), mar = c(5,6,2,2))
plot(preds, resids, xlab = "Predicted", ylab = "Residuals",
     main = "Residuals versus predicted")
abline(h = 0)
# QQ probability plot
qqnorm(resids, ylab="Residuals", xlab="Normal Scores",
       main="QQ plot")
qqline(resids)

par(mfrow = c(1,1), mar = c(5,4,4,2))

# Create a subset of the original dataset
exp_subset2 <- exp[which(exp$Question <5 & exp$Seconds < 98 & exp$Correct == 1),]
summary(exp_subset2)

# Scatterplot
plot(exp_subset2$Question, exp_subset2$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
# Fit and summarise the slr model fit to the data
lm3 <- lm(Seconds ~ Question, data = exp_subset2)
summary(lm3)

# Scatterplot with lm fit
plot(exp_subset2$Question, exp_subset2$Seconds, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
abline(lm3)


# Store the residuals, standardised residuals and predicted values
resids <- resid(lm3)
sresids = rstandard(lm3)
preds <- predict(lm3)
# Plot the residuals
par(mfrow = c(1, 2), mar = c(5,6,2,2))
plot(preds, resids, xlab = "Predicted", ylab = "Residuals",
     main = "Residuals versus predicted")
abline(h = 0)
# QQ probability plot
qqnorm(resids, ylab="Residuals", xlab="Normal Scores",
       main="QQ plot")
qqline(resids)

par(mfrow = c(1,1), mar = c(5,4,4,2))

#other analysis

# weighted least squares model
# analyse full data with changed point regression
# adjust for non-constant variables
# cooks distance - residuals vs leverage
# Read the data into R
exp <- read.csv(file='hmm.csv')
# View the first few lines of the data
head(exp)

# Scatterplot
plot(exp$Some, exp$Other, main = "Experiment time versus group",
     xlab = "Question", ylab = "Time to answer (seconds)")
# Fit and summarise the slr model fit to the data
lm1 <- lm(Something ~ Other, data = exp)
summary(lm1)

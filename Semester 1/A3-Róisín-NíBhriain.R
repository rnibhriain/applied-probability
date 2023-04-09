#Q1
lamps <- function (average, k) {
  
  # sets an empty array for the results of the trials
  results <- array(0, 10000)
  
  # run 10,000 samples
  for (i in 1:10000) {
    
    # empty array for the times t1 ... tk
    times <- array(0, k)
    
    # get the time for each t
    for (j in 1:k) {
      
      # use the exponential function to get the time
      times[j] <- rexp(1, 1/average)
      
    }
    
    # get the percentage using the formula
    results <- 100 * (times[1]/sum(times))
    
  }
  
  # return the mean of all 10000
  return <- mean(results)
  
}

par(mfrow=c(2, 3))  # divide graph area in 6 areas

# sets an empty array for the results of the trials
results <- array(0, 200)

# sets an empty array for the means of the trials
themeans <- array(0, 4)

# the sequence of ks
ks <- seq(2, 5, 1)

# use each k in the sequence
for (k in ks) {
  
  # run the function 200 times
  for (i in 1:200) {
    
    # get the result of the function
    results[i] <- lamps(24000, k)
    
  }
  
  # print the histogram of the 200 interations for each k
  hist(results, main = capture.output(cat('The Results For:', k)) , xlab="Percentage", col='black')
  
  # assign the mean of the 200 iterations into the mean result array
  themeans[k-1] <- mean(results)
  
  # print the mean
  print(mean(results))
}

# Plot the means for each k should be 1/k
plot(ks, themeans, main ='The Mean Results', xlab="K", ylab="The Means", col=rainbow(4), type='h', lwd=10)


#Q2 Normal Distribution

normalrandoms <- function (sigma, a, b) {
  
  # return the probability
  return <- pnorm(2, sd=sigma) - pnorm(1, sd=sigma)
  
}

# sets an empty array for the means of the trials (10000 was too large and crashed the application)
theprobabilities <- array(0, 1000*200+1)

par(mfrow=c(1, 1))  # divide graph area in 1 area

# start the count of the array
a <- 0

# sequence of the sigmas to choose from 1000 samples * 200 iterations (10000 was too large and crashed the application)
sigmas <- seq(1, 2, 1/(1000*200))

# run through each sigma
for (sigma in sigmas) {
  
  # call the function to get the probability of the current sigms
  theprobabilities[a] <- normalrandoms(sigma, 1, 2)
  
  # print each probability
  print(mean(theprobabilities[a]))
  
  # add one to the count
  a <- a + 1
}

# Plot the means for each a where sigma 1.47 is highest
plot(sigmas, theprobabilities, main ='The MResults', xlab="Sigma", ylab="The Probabilities", col=rainbow(1000*200), type='h')

# finds the index of the max mean
themax <- which(theprobabilities==max(theprobabilities))

print(sigmas[themax]*sigmas[themax]) # this is the required variance: approx 2.16


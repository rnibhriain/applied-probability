# Q1 Poisson Distribution

poissonfunction <- function (k, beta, timeperiod) {
  
  # sets an empty array for the results of the trials
  results <- array(0, 10000)
  
  # run 10,000 samples
  for (i in 1:10000) {
    
    # use the exponential function to get the time
    time <- rexp(1, 1/(timeperiod/beta))
    
    # put the time over the time period
    fractionoftime <- time/timeperiod
    
    # the answer to the poisson function using beta and fraction of time
    answer <- rpois(1, beta*fractionoftime)
    
    # if the answer is less than or equal to 0 then return 1
    if (answer <= 2) {
      results[i] <- 1
    } else {
      results[i] <- 0
    }
    
  }
  
  # return the mean of the array if samples
  return <- mean(results)
  
}

# sets an empty array for the results of the trials
results <- array(0, 200)


# run 200 times
for (i in 1:200) {
  
  # get the results of the function into the array
  results[i] <- poissonfunction(2, 5, 30)
  
}

par(mfrow=c(1, 2))  # divide graph area in 2 columns

# plot all 200 results for the function
plot(results, main ='The 200 Results', xlab='i', ylab='The Results', type='h', col = 'green')

# plot the mean of the 200 results of the function
plot(mean(results), main ='The Mean Result', xlab='The Mean', ylab='The Results', type='h', col = 'red')
print(mean(results))



#Q2

estimation <- function(a) {
  
  # sets an empty array for the results of the trials
  results <- array(0, 10000)
  
  # run 10,000 samples
  for (i in 1:10000) {
    
    # get the sample from 0 to a
    thesample <- runif(1, 0, a)
    
    # if statement for returning the E[Y] in the array
    if (thesample < (a/2)) {
      results[i] <- thesample
    } else {
      results[i] <- (a/2)
    }
  }
  
  # return the array of results for the 10,000 samples
  return <- results
   
}

# sets an empty array for the results of the trials
results <- array(0, 200)

# sets an emoty array for the means of the trials
themeans <- array(0, 5)

par(mfrow=c(2, 3))  # divide graph area in 6 areas

for (a in 1:5) {
  
  # run 200 times
  for (i in 1:200) {
    
    # place the mean of the 10000 random samples in the results array
    results[i] <- mean(estimation(a))
    
  }
  
  # print the histogram of the 200 interations for each a
  hist(results, main = capture.output(cat('The Results For:', a)) , xlab="Y", col='blue')
  
  # Place the mean of the 200 interations in the mean array
  themeans[a] <- mean(results)
}

# Plot the means for each a should be 3a/8
plot(themeans, main ='The Mean Results', xlab="a", ylab="The Means", col=rainbow(5), type='h', lwd=10)


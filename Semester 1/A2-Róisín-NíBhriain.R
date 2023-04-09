#Q1 - Rolling the Dice

roll <- function(n) {
  rolls <- matrix(0, 1, n) # set of of numbers
  
  #rolls the dice 10000 times
  for ( i in 1:10000) {
    
    # keep going till all of the numbers have been rolled
    x <- sample.int(n, 1, replace = TRUE)
    
    rolls[x] <- 1 # mark the number as rolled
    
    # return the number of times rolled if the set of numbers have all been seen
    ifelse(sum(rolls) == n, return(i), -1) 
  }
  
}

# sets an empty array for the results of the trials
results <- array(0, 200)

# run the trial 200 times
for (i in 1:200) {
  
  # run the roll function to roll the dice 10000 times
  results[i] <- roll(6)
  
}

# finds the average result of all the trials
DiceEndResult <- mean(results)

# prints the average result from all 200 trials
print(DiceEndResult)

print(results)

#Q2 - Picking Cards

pick <- function(k) {
  probability <- matrix(0, 1, 10000) # set of of numbers
  
  # guesses 10000 times
  for (i in 1:10000) {
    
    # places the cards face down
    theCards <- array(sample(1:k, k, replace = FALSE))
    
    # picks a set of numbers
    thePicks <- array(sample(1:k, k, replace = FALSE))
    
    #set an empty array for the results
    theResults <- array(0, k)
    
    # check each of the cards against the guesses
    for (j in 1:k) {
      if (theCards[j] == thePicks[j]) {
        theResults[j] <- 0
      } else {
        theResults[j] <- 1
      }
    }
    
    # set the default that all the picks are wrong
    allWrong <- TRUE
    
    # run through each result and if it is correct then set the allWrong variable as False
    for (j in 1:k) {
      if (theResults[j] == 0) {
        allWrong <- FALSE
      }
    }
    
    # if the results are all wrong then set the array location as 1 otherwise set it as 0
    if (allWrong) {
      probability[i] <- 1
    } else {
      probability[i] <- 0
    }
    
  }
  
  # return the mean of the array of probabilities
  return(mean(probability))
  
}

# sets an empty array for the results of the trials
results <- array(0, 200)

par(mfrow=c(2, 3))  # divide graph area in 2 columns

# set an empty array for the mean result of each k
meanResults <- array(0, 5)

# set an array of the steps of k
theSteps <- array(6:10, 5)

for (k in 6:10) {
  
  # run each k 200 times
  for (i in 1:200) {
    
    # run the roll function to roll the dice 10000 times
    results[i] <- pick(k)
    
    # print her location in the array
    print(i)
  }
  
  # set the mean result for each k
  meanResults[k-5] <- mean(results)
  
  # print the histogram of the probabilities of each k
  hist(results, main = capture.output(cat('The Results For:',k)) , xlab=k, col='blue')
}

# print the plot of the Mean Results for each k
plot(x=theSteps, y=meanResults, main ='The Mean Results', xlab='The Steps Of K', ylab='The Results', type = 'h', col = 'red')


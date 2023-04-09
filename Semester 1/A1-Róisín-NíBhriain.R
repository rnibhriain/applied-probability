library(numbers)

# sets the number of trials to 200
numberOfTrials <- 200

# sets an empty array for the results of the trials
results <- array(0, 200)

# run the trial 200 times
for (i in 1:200) {
  
  # create two samples
  x1 <- sample(1:10000, 10000, replace = TRUE)
  x2 <- sample(1:10000, 10000, replace = TRUE)
  
  # a for loop to check each element in the samples
  for (j in 1:10000) {
    
    # insert the results of the coprime function with the samples in the index of results
    results[i] <- coprime(x1[j], x2[j])
  }
  
  # check where you are in the loop
  print("got here")
  print(i)
}

# finds the average result of all the trials
endResult <- mean(results)

# prints the average result from all 200 trials
print(endResult)


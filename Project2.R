## Introduction to R Programming - Assignment 2 
## GUID: 2383746W


## Task 1 (Matrix) ---------------------------------------------------------------------------------------------------------------

# 1 - Create matrix "battery" 
battery <- matrix(c(134.75, 57.25, 57.5, 155.75, 119.75, 198, 144, 145.75, 85.5),
                  nrow=3, ncol=3, byrow = TRUE)

# 2 - Give associated names to the rows and cols of battery
rownames(battery) <- c("Material A", "Material B", "Material C")
colnames(battery) <- c("Temeprature Low", "Temperature Medium", "Temperature High")
battery

# 3 - Compute mu hat
mu.hat <- mean(battery)

# 4 - Compute alpha hat and beta hat
alpha.hat <-apply(battery, 1, mean)-mu.hat 
beta.hat <- apply(battery, 2, mean)-mu.hat

# 5 - Compute matrix of residuals 
est.resid <- sweep(sweep(battery-mu.hat, 1, alpha.hat, "-"), 2, beta.hat, "-")

# 6 - Compute residual sum of squares 
rss <- sum(est.resid^2)

# 7 and 8 - Compute F-tests for material types and for temperature  
m <- nrow(battery)
n <- ncol(battery)

# Sum of squares for alpha and beta 
ss.alpha <- n*sum(alpha.hat^2)
ss.beta <- m*sum(beta.hat^2)

# Mean sum of squares for alpha, beta and the residuals 
mss.alpha <- ss.alpha/(m-1)
mss.beta <- ss.beta/(n-1)
mss.resid <- rss/((m-1)*(n-1))

# Compute the F-tests for alph and beta
f.alpha <- mss.alpha/mss.resid 
f.beta <- mss.beta/mss.resid

# Test conclusion 
ifelse(c(f.alpha, f.beta) > c(4.46, 4.46), 
       c("significant evidence for an effect of material types",  "significant evidence for an effect of temperature"), 
       c("no significant evidence for an effect of material type",  "no significant evidence for an effect of temperature"))


## Task 2 (Ices Data) -------------------------------------------------------------------------------------------------------------

ices <- readRDS("ices.RDS")

# 1 - Replace all NA's by 0
ices[is.na(ices)] <- 0

# 2 - Determine total catch for each subdivision
tot.catch.sub <- as.data.frame(apply(ices, 1, sum))
colnames(tot.catch.sub) <- "total catch"
tot.catch.sub

# 3 - Remove subdivisions for which total catch is 0
ices <- subset(ices, tot.catch.sub!=0)

# 4 - Determine total catch for each species
sum.species <- apply(ices, 2, sum)
tot.catch.species <- data.frame(sum.species)

# 5 - Determine three most abundant species  
most.abund <- sum.species[order(sum.species, decreasing=TRUE)[1:3]]

# 6 - Convert table of total catches to table of percentual catches 
relative.catches <- sweep(ices, 1, rowSums(ices), "/")*100
rel <- data.frame(relative.catches)

# 7 - Determine the most abundant species for each subdivision 
most <- apply(ices, 1, which.max)
most.abund.sub <- data.frame(rownames(ices), colnames(ices)[most])
colnames(most.abund.sub) <- c("subdivisions", "species")


## Task 3 (Mean Shift Algorithm)  -------------------------------------------------------------------------------------------------------------

x <- readRDS("gamma.RDS")

## Set up the algorithm with a while loop
mean.shift <- function(x, h=sd(x)/length(x)^(0.2), it=100) { 
  mu <- median(x)                                              # starting value of mu
  k <- 1                                                       # start iteration at k=1 
  mu.old <- 0                                                  # assign 0 to mu old (mu from previous iteration k-1)
  
 if (h==0) {stop("h cannot be zero")}                          # set the given error conditions, stops loop if it is evaluated as TRUE 
 if (it<=0) {stop("it must be a positive integer")}
  
 while (k<=it & abs(mu-mu.old) >= 1e-8) {                      # repeat the iterations while k is smaller or equal to 100 or the convergence is larger than 10^-8
   mu.old <- mu                                                # update the mu from previous iteration k-1, starting at the median value for k=1         
   w <- exp(-(x - mu)^2 / (2*h^2))                             
   mu <- sum(w*x) / sum(w)                                     # update mu from the current iteration k 
   k <- k + 1                                                  # start next iteration step at k+1 and repeat loop
  }
  return(mu)                                                   # return loop while conditions are satisfied
}
mean.shift(x)                                                  # check output of the mean.shift fucntion for given parameters
mean.shift(x, h=0)
mean.shift(x, h=1.2)
mean.shift(x, it=-10)
mean.shift(x, it=4)
mean.shift(c(1,1,2), it=1)


### Task 4 (Collatz Algorithm) --------------------------------------------------------------------------------------------------------

## Set up the algorithm with an for loop 
collatz <- function(n, length=30) {
  seq <- numeric(length)                    # create an empty sequence with 30 positions 
  seq[1] <- n                               # set the first sequence entry to the input number n 
 
 if (n<=0) {stop("Input not positive")}     # set the given error conditions, stops loop if evaluated as TRUE
 if (n%%1!=0) {stop("Input not integer")}
 
  for (i in 2:length) {                     # start the loop at the second iteration and end it at the 30th iteration
   if (seq[i-1]%%2==0) {                    # if the previous vector term is is even, set next term to a half of the previous term 
     seq[i] <- seq[i-1]/2
     } else {                               # else, if previous term is not even, next term will be 3 times the previous term minus 1
       seq[i] <- 3*seq[i-1]+1
     }
   if (seq[i]==1) {                         # if the term reaches the number 1, stop the algorithm 
   break 
     }
  }
  seq <- seq[1:i]                           # create the sequence until 1 is reached or we have reached 30 iterations
  return(seq)
}
collatz(14)                                 # check output of the collatz function for given parameters
collatz(2)
collatz(17,5)
collatz(10001)
collatz(0.8)
collatz(-2)

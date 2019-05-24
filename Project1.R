#### Assignment 1 - Introduction to R Programming ####

## set up directory 
setwd("C:/Users/ernst/Desktop/Assignment1")

## Task 1 (Monte Carlo Integration) --------------------------------------------------------------------------------------

# 1. Create data frame U with random numbers from Unifom distribution
n <- 10000                                   # number of total points 
U <- data.frame(cbind(runif(n), runif(n)))   # create a data frame with 2 columns and 10000 rows 

# 2. Give column names x and y to data frame U
colnames(U)[1:2] <- c("x", "y")              # give column names where first column is x and second y 

# 3. Add column to U named as "fx" for the function values of x
x <- U[, 1]                                    # define data points in x as vector  
fx <- 1-sqrt(exp((sin(pi*x))^4))*cos(pi*x)^6   # define function for every data point in x 
U <- cbind(U, fx)                              # add a third column called fx for the density of x 

# 4. Calculate proportion of rows of U for which y <= fx 
y <- U[, 2]                                  # define data points in y as vector 
s.rows <- nrow(U[y<= fx, 2:3])               # number of rows for which y is smaller than fx
t.rows <- nrow(U)                            # total number of rows in data frame U 
prop <- s.rows/t.rows                        # proportion of rain drops falling below fx (pink area)

## Task 2 (dataset starwars) --------------------------------------------------------------------------------------

# 1. Read in data set called starwars 
starwars <- read.csv("starwars.csv", header=TRUE)
head(starwars) 
str(starwars)

# 2. Add column BMI to dataset 
starwars <- transform(starwars, BMI=10000*(starwars$mass)/(starwars$height)^2)  
head(starwars)

# 3. Find character with highest BMI 
max_bmi <- paste(starwars$name[which.max(starwars$BMI)])    # display character name without displaying factor levels 
                             
# 4. Find how many characters come from every homeworld
char.counts <- as.data.frame(table(starwars$homeworld))     # create a data frame of with absolute frequencies of names per homeworld
colnames(char.counts)[1:2] <- c("homeworld", "frequency")   # rename first columns of table to homeworld and frequency
char.counts    

# 5. Find names of all characters which are from same homeworld as Chewbacca
chewbacca.home <- paste(starwars$homeworld[starwars$name=="Chewbacca"])   # first identify Chewbaccas homeworld using paste to get a character back 
paste(subset(starwars, homeworld==chewbacca.home)$name)                   # return names of charatcers from the same homeworld as Chewbacca

# 6. Identify all characters taller than 2 metres 
paste(starwars$name[which(starwars$height > 200)])  # return character names without factor levels

## Task 3 (p.m.f) --------------------------------------------------------------------------------------------------------------- 

# 1. Create vectors x and p 
x <- 1:9
p <- log(1+1/x, base=10)

# 2. Ensure that p sums to 1 for a valid pmf
sum(p)

# 3. Identify which x entry maximises p
x[which.max(p)]

# 4. Compute the expected value of x
x.exp <- sum(x*p)
x.exp

# 5. Compute the variance of x 
x.var <- sum((x-x.exp)^2*p)
x.var

## Task 4 (p.d.f) -----------------------------------------------------------------------------------------------

# 1. Create vector x of length 100 
x <- seq(0, 2, length.out=100)

# 2. Create Weibull density function using the built in fucntion  
y <- dweibull(x, shape=5, scale=1)  # where scale=lambda=1 and shape=k=5 

# 3. Plot the Weibull density function 
plot(y ~ x, type="l")

# 4. Add another two Weibull pdf's with different parameter values into plot 
y1 <- dweibull(x, shape=0.5 , scale=1)  # with k=0.5 and lambda=1
y2 <- dweibull(x, shape=1.5 , scale=1)  # with k=1.5 and lambda=1

# create data frame "graphs" to store all weibull densities and combine them into the same plot  
graphs <- as.data.frame(cbind(x, y, y1, y2)) 
matplot(graphs$x, graphs[,-1], type="l", col=c("black", "blue", "green"), lty=1,
        xlim=range(x), ylim=range(x), xlab="x", ylab="y")                                 

# 5. Add a legend to the previous plot 
legend("topright", lty=1, legend=c("lambda=1, k=5", "lambda=1, k=0.5", "lambda=1, k=1.5"),
       col=c("black", "blue", "green"))

## Task 5 (Trump tweets) ------------------------------------------------------------------------------------------------

# 1. Code to load data corretly 
trump <- read.table("trump.txt", header=TRUE, sep="")
View(trump)
str(trump)
head(trump)

# 2. Add column nwords_new to data set 
trump <- transform(trump, nwords_new=cut(nwords, 
                   breaks=c(0, 15, 25, Inf), labels=c("short", "medium", "long")))  
trump

# 3. Create barplot with number of tweets from each source Android and IOS
n.an <- nrow(subset(trump, source=="Android"))  # number of tweets from Android 
n.io <- nrow(subset(trump, source=="iOS"))      # number of tweets from iOS
height <- c(n.an, n.io)                         # height of the bars 
names(height)=c("Android", "iOS")               # name of the bars 
barplot(height)  

# 4. Show number of tweets from each source by sentiment 
n.tps <- table(trump$source, trump$sentiment)             # show number of tweets for each sentiment for Android and iOS 
my.cols <- c(palette()[2], palette()[4])                  # creation of colour vector with my.cols=c("red", "blue")
barplot(n.tps, beside = TRUE, las=2, col=my.cols)         # create barplot where bars for each source are displayed beside each other 
legend("topright", fill = my.cols, c("Android", "iOS"))   # crate legend 

# 5. Plot density for both sources in regards to tweet sending time   
plot(density(trump$hour[(trump$source=="Android")]), col=my.cols[1], 
     main="Trump likes to tweet in the early morning")
lines(density(trump$hour[trump$source=="iOS"]), col=my.cols[2])
legend("topright", lty=1, col=my.cols, c("Android", "iOS"))

# 6. Create Boxplot with number of words for each source 
n.wan <- trump$nwords[trump$source=="Android"]  # number of words for Android 
n.wio <- trump$nwords[trump$source=="iOS"]      # number of words for iOS
box.names <- c("Android", "iOS")                # name vector for both sources 
boxplot(n.wan, n.wio, names=box.names)          # creating the boxplots for the number of words for both sources 
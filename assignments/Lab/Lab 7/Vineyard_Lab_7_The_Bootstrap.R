#apply() attempts to replace the need for for-loops
#apply() works on data frames or other 2D arrays of data like matrices
#ARGUMENTS: x- the 2D data, MARGIN- whether to apply the function to rows (MARGIN=1)
  #or columns (MARGIN=2), FUN- function to apply to the rows or columns

dat=matrix(1:49, nrow=7, ncol=7)
dat

#Minimum and maximum values in each row
apply(dat, MARGIN=1, FUN=min)
apply(dat, MARGIN=1, FUN=max)

#Mean values in each column
apply(dat, MARGIN=2, FUN=mean)

#Data Files
require(here)
moths=read.csv(here("data", "moths.csv"))
head(moths)

#BOOTSTRAP: resampling with replacement on a single sample of observations to 
  #simulate the process of taking many samples from a population
  #Creates a kind of alternative distribution
#Monte-Carlo resampling can characterize a null distribution

#t-distribution is essentially a standard normal distribution that's been adjusted 
  #for use with a finiate sample size
#t-distribtuion's heavy tails represent the greater uncertainty in the t-distribution
#If your sample has 30 or less individuals you should use a t-distribution


#Create the results vector
m=10000
#numeric() creates a vector of length m with all values initialized to 0
result=numeric(m)
head(result)

#Perform the bootstrap
for(i in 1:m)
{
  result[i]=mean(sample(moths$anst, replace=TRUE))
}

#Calculate the quantiles
mean(result)

quantile(result, c(0.025, 0.975))

#Bootstrap Interval Using boot()
install.packages("boot")
require(boot)
#R= number of resamplings you want
#data= data object you want to resample
#statistic= function that returns the statistic of interest

#Custom Mean Function
boot_mean= function(x,i)
{
  return(mean(x[i], na.rm=TRUE))
}
#x in this function is the data in boot()
#i in this function is used to select random assortments of x

myboot=
  boot(
    data=moths$anst,
    statistic=boot_mean,
    R=10000)
myboot
#original is the original mean of the whole sample
#bias is the difference between the original mean and the mean of the bootstrapped
  #samples
#std.error is the standard deviation of the simulated values

str(myboot)

mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

quantile(
  myboot$t,
  c(0.025, 0.975))

#Setting up the bootstrap
moth_dat= moths[,-1]
head(moth_dat)

n=nrow(moth_dat)
m=100
moth_result= matrix(nrow=m, ncol=n)

##Running the bootstrap simulation
n=nrow(moth_dat) #number of rows or sample observations
m=100 #number of bootstrap iterations
moth_result=matrix(nrow=m, ncol=n)

#the outer loop: runs once for each bootstrap iteration. index variable is i
for(i in 1:m)
{
  #the innter loop: simulates increasing sampling intensity
  #sampling intensity ranges from 1 site to the complete count of sites (24),
  #index variable is j
  for(j in 1:n)
  { 
    #Sample the input data row indices, with replacement
    rows_j= sample(n, size=j, replace=TRUE)
    #creates a new data matrix from the resampled rows
    t1=moth_dat[rows_j, ]
    #calculates the column sums of the new data matrix
    t2=apply(t1, 2, sum)
    #counts the number of columns in which any moths were observed
    moth_result[i, j]= sum(t2 > 0)
  }
}
head(moth_result)


#Packaging your code into a function
#First Draft
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#SECOND DRAFT
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)


###CORRECT FUNCTION####
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  n=nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
      
      t1 = input_dat[rows_j, ]
      
      t2 = apply(t1, 2, sum)
      
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

#Building the rarefaction curve
moths=read.csv(here("data", "moths.csv"))
rarefact=rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

rare_mean=apply(rarefact, 2, mean)
rare_quant=apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare=t(rbind(rare_mean, rare_quant))

#Plotting the curve
matplot(
  rare,
  type="l",
  xlab="Number of sampling plots",
  ylab="Species richness",
  main="Rarefaction Curve")

legend(
  "bottomright",
  legend=c("mean","2.5%","97.5%"),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))



####QUESTIONS###

#Question 1
require(palmerpenguins)
dat_gentoo_all= subset(penguins, species =="Gentoo", na.rm=TRUE)
dat_gentoo_bill_length= na.omit(dat_gentoo_all$bill_length_mm)
length(dat_gentoo_bill_length)

#Question 2
sd(dat_gentoo_bill_length)

#Question 3 critical t-values for 95% CI
qt(c(0.025, 0.975), 122)


#Question 4
#sample standard error= sample standard deviation/sqrt(sample size)
sse_mean= function(x)
{sse= sd(x, na.rm=TRUE)/(sqrt(length(x[! is.na (x)])))
return(sse)
}

sse_mean(dat_gentoo_bill_length)

#Question 5 CI 
#radius=
qt(0.975, 122) * (sse_mean(dat_gentoo_bill_length)/sqrt(123))

#CI= mean +- radius
mean(dat_gentoo_bill_length) + 
  qt(c(0.025, 0.975), 122) * (sse_mean(dat_gentoo_bill_length)/sqrt(123))

#Question 6-8 bootstrap 95% CI
require(boot)
myboot = 
  boot(
    data = dat_gentoo_bill_length,
    statistic = boot_mean,
    R = 10000)
myboot

quantile(myboot$t, c(0.025, 0.975))


#Question 9
n_iterations=10000

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  n=nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
      
      t1 = input_dat[rows_j, ]
      
      t2 = apply(t1, 2, sum)
      
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefaction_sampler(moths [,-1], 10000)


rarefact_moths=rarefaction_sampler(moths[,-1], 10000)
rare_mean=apply(rarefact_moths, 2, mean)
rare_quant=apply(rarefact_moths, 2, quantile, probs=c(0.025, 0.975))
rare=t(rbind(rare_mean, rare_quant))

help("matplot")

matplot(
  rare,
  type="l", lwd=c(2, 2, 2), lty=c(1,2,4),
  col=brewer.pal(n=3, "Dark2"), 
  xlab="Number of Sites Sampled",
  ylab="Average Number of Species Observed",
  main="Rare Moth Species Found in MA\nSoutheast Pine Barrens")

legend(
  "bottomright",
  legend=c("Mean","Lower Quantile (2.5%)","Upper Quantile (97.5%)"),
  lty=c(1,2,4), lwd=c(2,2,2),col=brewer.pal(n=3, "Dark2"), inset=c(.2,.2),
  title="Legend", title.col="Black", title.adj = .4, 
  seg.len = 4)
  
  
help(legend)


#Couldn't get this package of colors to display
install.packages("RColorBrewer")
library("RColorBrewer")
display.brewer.all()



#SOMETHING MUCH LATER WE'LL NEED THE QT FUNCTION

#calculate critical t-values
  #tells us at which value along the x-axis would I find the 97.5% percentile

n=nrow(penguins)
alpha=0.05
qt(1-alpha/2, n-1)
qt(alpha/2, n-1)
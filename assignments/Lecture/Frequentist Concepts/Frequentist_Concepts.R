#Question 1
#observing a count of exactly 3 successes, n=4, p=0.75
dbinom(3, 4, 0.75)

#Question 2
#observing a count of 3 or less, n=4, p=0.75
#3 or less includes 3
pbinom(3, 4, 0.75)

#Question 3
#observing more than 3, n=5, p=0.75
#more than 3 does not include 3
1- pbinom(3, 5, 0.75)
#CHECK: both give the same number
1- (dbinom(0, 5, 0.75) + dbinom(1, 5, 0.75) + dbinom(2, 5, 0.75) + dbinom(3, 5, 0.75))
dbinom(4, 5, 0.75) + dbinom(5, 5, 0.75)

#Question 4
#observing a value less than 1.2, mean=2, std=2
pnorm(1.2, mean=2, sd=2) #THIS would give 1.2 and less
#is this the same as less than 1.2?

#Question 5
#observe a value greater than 1.2, mean=2, std=2
1-pnorm(1.2, mean=2, sd=2) #THIS would still include 1.2

#Question 6
#possibly do the prob of 3.2 minus the prob of 1.2
pnorm(3.2, mean=2, sd=2) - pnorm(1.2, mean=2, sd=2)

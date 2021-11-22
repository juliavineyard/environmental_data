dpois(x = 7, lambda = 10.4)

#Question 1: Binomial Parameters
help("dbinom")
#First argument is the number of successes
#n=number of trials, p=probability
#n=6, prob=2/3

#Question 2: Binomial Probability 1
dbinom(4, 6, 2/3)

#Question 3: Binomial Probability 2
dbinom(0, 6, 2/3)

#Cumulative Probability: The p-functions
#used ppois to ask what the probability that I observe a count of 7 or fewer
  #if I have a poisson-distributed population with lambda=10.4
ppois(q = 7, lambda = 10.4)

#Law of Total Probability and Complementary Events
  #I can also use it to ask what the probability of observing a count greater
    #thank 7 using the law of total probability
1 - ppois(q = 7, lambda = 10.4)

#To get the probability of observing seven or greater
1 - ppois(q = 6, lambda = 10.4)

#Question 4: Binomial Probabilities 3
  #pbinom tells us the probablity of seeing the set number or fewer
  #Calculate the probability of observing 4 or fewer presences in the 6 plots
pbinom(4, 6, 2/3)

#Question 5
  #use pbinom and the law of total probability to calculate the probability of
    #observing four or more presences in the 6 plots
#pbinom(3,...) will tell us the probablity of 3 or fewer
1- pbinom(3, 6, 2/3)

  #Test that Question 5 is correct
  dbinom(4, 6, 2/3) +
  dbinom(5, 6, 2/3) +
  dbinom(6, 6, 2/3)  
  
  
  #pbinom is the area under the curve (cumulative density)
  #dbinom tells us the probability of a specific x value (density at a point)
x_observed=c(2,6)
x_observed

dpois(x=2, lambda=4.5)
dpois(x=6, lambda=4.5)

dpois(x=2, lambda=4.5) * dpois(x=6, lambda=4.5)

wiwa_counts = c(2,6)
dpois(x=wiwa_counts, lambda = 4.5)

prod(dpois(x=wiwa_counts, lambda=4.5))

sum(log(dpois(x=wiwa_counts, lambda=4.5)))

require(here)
dat_bird=read.csv(here::here("data", "bird.sta.csv"))
dat_habtitat=read.csv(here("data", "hab.sta.csv"))
dat_all=merge(dat_bird, dat_habtitat)

summary(dat_all$WIWA)
hist(dat_all$WIWA, breaks=7)
#^This histogram groups counts of 0 and 1 together!

#This method allows us to get the correct number of bins, but still has 0 and 1
  #counts together
hist(dat_all$WIWA, breaks=0:7)

#Un-group counts of 0 and 1 so that we see them as distinct bars,
  #and the bars are now centered over the census counts
hist(dat_all$WIWA, breaks=0:7 -0.5)

#Histograms with discrete count data and don't know maximum value
par(mfrow=c(1,2))
dat=dat_all$WIWA
hist(dat, breaks=0:(max(dat) + 1) -0.5, main="Histogram of\nWilson's Warbler Counts")
dat=dat_all$GRJA
hist(dat, breaks=0:(max(dat) +1) -0.5, main="Histogram of\nGray Jay Counts")

#WIWA sum of log-likelihoods
sum(log(dpois(x=dat_all$WIWA, lambda = 1.0)))


#QUESTION 1
wiwa_counts
sum(log(dpois(x=wiwa_counts, lambda=4.001)))

mean(wiwa_counts)
sum(log(dpois(x=wiwa_counts, lambda= 4)))

#QUESTION 2
wiwr_counts=dat_all$WIWR
sum(log(dpois(x=wiwr_counts, lambda=0.99)))

mean(wiwr_counts)
sum(log(dpois(x=wiwr_counts, lambda= 1.456023)))

#QUESTION 3
  #Binomial- has a fixed number of trials
summary(wiwr_counts)
  #tells us that the site that had the most WIWR observed is 6
    #what do we want to assume is the max possible amount to observe?
    #your n parameter is the max that you expect to see
      #Can use your data's max or another number you may know to expect
n=20 

n * x = 1.45 #(1.45 came from the mean from summary(wiwr_counts))
prob_guess= 1.45/n

sum(log(dbinom(wiwr_counts, size=n, prob=prob_guess)))

#QUESTION 4




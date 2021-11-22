#for 95% CI
qnorm(c(0.025, 0.975))


#Question 1, for 90% CI
qnorm(c(0.05, 0.95))

#Question 2
help(qt)
#qt(p:vector of probabilities, df:degrees of freedom, ncp: non-centrality parameter delta)
qt(c(0.025, 0.975), 10)

#Question 3 & 4
qnorm(0.025)
qt(0.025, 61) #Question 3 answer
qt(0.025, 473) #Question 4 answer
qt(0.025, 100000) #gets you very close
qt(0.025, 65000) #gets you right at 1.96

#Question 5
mean=10
#radius= t-critical value * standard error

#Standard error
3.14/sqrt(50)

#Critical t-values
qt(c(0.025, 0.975), 49) #Answer to #5


#Question 6
#10 + radius and 10-radius
2.009575 * (3.14/sqrt(50)) #=radius

10 - 0.892378
10 + 0.892378 

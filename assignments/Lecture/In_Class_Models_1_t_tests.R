require(palmerpenguins)
dat_ade=droplevels(subset(penguins, species=="Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass",
     xlab="body mass (g)")

#Question 1
boxplot(dat_ade$body_mass_g ~ dat_ade$sex,
        main= "Body Mass of Adelie Penguins",
        ylab="Body Mass (g)")

#Question 2
help("t.test")

pen_fem=droplevels(subset(dat_ade, sex=="female"))
t.test(pen_fem$body_mass_g, mu=0)
#took out alternative=greater since this was just asking for different than 0

#Question 3
#Since the p value is so small (< 2.2e-16), we can conclude that this number is
  #significant, and female body mass is likely greater than 0.

#Question 4
pen_male=droplevels(subset(dat_ade, sex=="male"))
t.test(pen_male$body_mass_g, alternative="greater", mu=4000)
#confidence interval goes up to infinity (inf) since it's a one tailed test
#don't have a high p-value because our mean of x falls within our CI

#Question 5
#Since our p-value is 0.1438 we cannot reject the null hypothesis that the true
  #mean of the Adelile male penguins is greater than 4000g

#Question 6
#both of these codes give us the same thing, just presented in a different way
t.test(pen_fem$body_mass_g, pen_male$body_mass_g, alternative="two.sided")
t.test(dat_ade$body_mass_g ~ dat_ade$sex, alternative="two.sided")
#p-value is super small so there is a significant difference between the male \
  #and female body mass\
#our CI does not contain 0 so we can reject our null hypothesis for a 2 sample test
#our CI is the estimate of the difference in body masses between the sexes

#Question 8
#males are heavier than females
t.test(dat_ade$body_mass_g ~ dat_ade$sex, alternative="l")


#Question 9
#Are females heavier than males? silly example
t.test(dat_ade$body_mass_g ~ dat_ade$sex, alternative="g")
#p-value of 1 = we would never expect to see what we have tested
#can't reject our null hypothesis that females are not heavier

#alternative is relative to the x 
levels(dat_ade$sex)
#checking these factor levels tell you what factor is going to be the base case
#you can also see when you run a t-test what factor appears first in the "mean in group"

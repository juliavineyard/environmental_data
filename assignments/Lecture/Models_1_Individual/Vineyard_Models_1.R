require(here)
catrate= read.csv(here("data", "catrate.csv"))

head(catrate)
summary(catrate)

hist(catrate$cat.rate)

#Check for Normality
shapiro.test(catrate$cat.rate)
#low p-value means that there is good evidence to reject the null
#for this a p-value less than 0.05 says we can reject the null that the
  #data were sampled from a normally-distributed population

#One-Sample Tests
mean(catrate$cat.rate)

#Parametric One-Sample Test: The t-test
#t-test Null= the mean of population from which the data were collected is not different from x
#default null has x=0

t.test(catrate$cat.rate, mu=2/7)

#One-sided Alternative Hypothesis
t.test(catrate$cat.rate, mu=2/7, alternative="g")

t.test(catrate$cat.rate, mu=2/7, alternative="l")

#Wilcoxon Rank Sum Test
wilcox.test(catrate$cat.rate, mu=2/7, exact=FALSE)

#Comparing two sample means
#the null hypothesis in a two-sample test is "There is no difference in mean values between the two groups
require(palmerpenguins)
penguin_dat=droplevels(subset(penguins, species !="Gentoo"))

summary(penguin_dat)

boxplot(penguin_dat$flipper_length_mm ~ penguin_dat$species, 
        ylab="Flipper Length (mm)")

#Testing for Normality
#low p-values suggest that there is good evidence to reject the null hypothesis that the data were sampled from a normally distributed population
shapiro.test(aggregate(flipper_length_mm ~ species, data=penguin_dat,
                       FUN=mean, na.rm=TRUE))

dat_adelie=subset(penguin_dat, species=="Adelie")
dat_chin=subset(penguin_dat, species=="Chinstrap")
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chin$flipper_length_mm)

#Parametric and Nonparametric Tests
t.test(flipper_length_mm ~ species, data=penguin_dat)
wilcox.test(flipper_length_mm ~ species, data = penguin_dat)

#QUESTION 1
hist(catrate$cat.rate, main="Salamander Reproduction Catastrophic Rates",
     xlab="Catastrophic Rate", col="sky blue")

#QUESTION 2
shapiro.test(catrate$cat.rate)
#p-value= 0.04097

#QUESTION 3
#The null hypothesis is that the data were sampled from a normally distributed population

#QUESTION 4
#Yes, because the p-value is less than 0.05 we can reject the null hypothesis 
  #and say that there is strong evidence that the sample came from a non-normally distributed population.

#QUESTION 5
t.test(catrate$cat.rate, mu=2/7)

#QUESTION 8
t.test(catrate$cat.rate, mu=2/7, alternative="g")

#QUESTION 9
# This gives you a mean for each group (species), but don't know how to apply it to the pond data
t.test(flipper_length_mm ~ species, data=penguin_dat)
t.test(catrate$cat.rate)
t.test(catrate$cat.rate, mu=2/7)


#QUESTION 1
wilcox.test(catrate$cat.rate, mu=2/7, alternative="g", exact=FALSE)

#QUESTION 16
dat_adelie=subset(penguin_dat, species=="Adelie")
dat_chin=subset(penguin_dat, species=="Chinstrap")
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chin$flipper_length_mm)

#QUESTION 18
require(RColorBrewer)
display.brewer.all()
help(brewer.pal)

require(here)
png(
  filename=here("images", "Models_1_histogram.png"), 
  width = 1500, height = 1000, 
  res=180)

par(mfrow= c(1,2))
hist(dat_adelie$flipper_length_mm,
     ylim = c(0,50),
     main="Adelie Flipper Length",
     xlab="Flipper Length (mm)",
     col= brewer.pal(n=3, "Paired"))

hist(dat_chin$flipper_length_mm,
     ylim = c(0, 20),
     main="Chinstrap Flipper Length",
     xlab="Flipper Length (mm)",
     col= brewer.pal(n=3, "Paired"))

dev.off()

#QUESTIONS 19 & 20
t.test(flipper_length_mm ~ species, data=penguin_dat)

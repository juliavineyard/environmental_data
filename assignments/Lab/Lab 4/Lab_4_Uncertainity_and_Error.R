#dnorm(): the probability density, height of the curve
#pnorm(): the cumulative probability density, area underneath the curve
#qnorm(): the quantile function
#rnorm(): function to generate random, normally-distributed numbers

#Practice Exercise: name the distribution function
dnorm(-1.96, mean=0, sd=1)
pnorm(-1.96, mean=0, sd=1)

# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x, mean=0, sd=1)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

#type="l" plots just lines
#type="p" plots just points
#type="b" plots both lines and points

#Penguins Example
require(palmerpenguins)
hist(penguins$body_mass_g, 
     main="Histogram of Penguin Body Mass",
     xlab="Body Mass (g)")
mean(penguins$body_mass_g, na.rm=TRUE)
sd(penguins$body_mass_g, na.rm=TRUE)
nrow(penguins)

#Random Penguin Masses
#set.seed requires that the same # of declared numbers
  #are used each time that the code is run
set.seed(1)
n_samples=344
pop_sd=802
pop_mean=4202

dat_1=rnorm(n=n_samples, mean = pop_mean, sd=pop_sd)
dat_2=rnorm(n=n_samples, mean = pop_mean, sd=pop_sd)
dat_3=rnorm(n=n_samples, mean = pop_mean, sd=pop_sd)
dat_4=rnorm(n=n_samples, mean = pop_mean, sd=pop_sd)

#Simulated Body Mass Histograms
par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

#Random Uniform Numbers
set.seed(12)
dat_unif = runif(n = 270, min = 0, max = 4)
hist(dat_unif)

#Randomness and Replication: set.seed()
set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

#Measuring Error
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

#Create the Data
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

#Fit a Linear Deterministic Model
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#Add predicted values
  #Why did I get different values than what was shown in walk-through?
    #Because he does not have a set.seed in the walk-through
y_predicted_vec=line_point_slope(dat$x, guess_x, guess_y, guess_slope)

dat$y_predicted=y_predicted_vec
dat

#Calcualte the residuals
  #the residuals are the difference between the predicted & observed values
resid_vec=dat$y_observed - dat$y_predicted
dat$resids=resid_vec
dat
sum(dat$resids)
sum(abs(dat$resids))

#Plot the residuals
par(mfrow=c(1,2))
plot(dat$resids)
hist(dat$resids)


#QUESTIONS 
#Q1
norm_mean=10.4
norm_sd=2.4

norm_17=rnorm(17, mean=norm_mean, sd=norm_sd)
norm_30=rnorm(30, mean=norm_mean, sd=norm_sd)
norm_300=rnorm(300, mean=norm_mean, sd=norm_sd)
norm_3000=rnorm(3000, mean=norm_mean, sd=norm_sd)

#Q2
require(here)
png(
  filename=here("images", "lab_04_hist_01.png"), 
    width = 1500, height = 1600, 
    res=180)

par(mfrow=c(2,2))
hist(norm_17, main="17 Random Points")
hist(norm_30, main="30 Random Points")
hist(norm_300, main="300 Random Points")
hist(norm_3000, main="3000 Random Points")

dev.off()

#Q7
# Generate a vector of x-values
require(here)
svg(
  filename=here("images", "norm_1.svg"),
  width=7, height=7, pointsize=12)
x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean=10.4, sd=2.4)

plot(x, y, main = "Standard Normal PDF \nMean=10.4 SD=2.4", type = "l", xlim = c(0, 20))
abline(h = 0)

dev.off()

#Q9
set.seed(2)
n_pts = 8
x_min = 0
x_max = 24
x = runif(n = n_pts, min = x_min, max = x_max)
dat_1 = data.frame(x = x, y_observed = rnorm(n_pts))

set.seed(4)
x = runif(n = n_pts, min = x_min, max = x_max)
dat_2 = data.frame(x = x, y_observed = rnorm(n_pts))

set.seed(6)
x = runif(n = n_pts, min = x_min, max = x_max)
dat_3 = data.frame(x = x, y_observed = rnorm(n_pts))

set.seed(8)
x = runif(n = n_pts, min = x_min, max = x_max)
dat_4 = data.frame(x = x, y_observed = rnorm(n_pts))

require(here)
png(
  filename=here("images", "random_data_lab_4.png"), 
  width = 1500, height = 1600, 
  res=180)
par(mfrow=c(2,2))
plot(dat_1$x, col="darkblue", main="Random 1")
hist(dat_2$x, col="blue", main= "Random 2")
boxplot(dat_3$x, col="lightblue", main="Random 3")
hist(dat_4$x, col="steelblue", main="Random 4")

dev.off()

#Q11
set.seed(2)
n_pts = 48
x_min = 0
x_max = 24
x = runif(n = n_pts, min = x_min, max = x_max)
dat_1 = data.frame(x = x, y_observed = rnorm(n_pts))

require(here)
png(
  filename=here("images", "random_data_model_fit_lab_4.png"), 
  width = 1500, height = 1600, 
  res=180)

plot(dat_1)

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

curve(line_point_slope(x, 12, 0, 0.05), add=TRUE)

dev.off()

#Q13
set.seed(2)
n_pts = 48
x_min = 0
x_max = 24
x = runif(n = n_pts, min = x_min, max = x_max)
dat_1 = data.frame(x = x, y_observed = rnorm(n_pts))

y_predicted_vec=line_point_slope(dat_1$x, 12, 0, 0.05)
dat_1$y_predicted=y_predicted_vec

resid_vec=dat_1$y_observed - dat_1$y_predicted
dat_1$resids=resid_vec
dat_1

#Q14

hist(dat_1$resids, xlab="Residuals")
plot(dat_1$y_predicted, dat_1$resids, xlab="Predicted", ylab="Residuals")
par(mfrow=c(1,2))

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

exp_fun = function(x, a, b)
{
  return(a*exp(-b*x))
}
curve(
  exp_fun(x, 0.3, 1/15),
  from=0, to=50, add=FALSE,
  main="Exponential function")

#Simulating data with different stochastic distributions
#Simulated data on a line
set.seed(24)
n_pts=50
x_min=2
x_max=10
x_sim=runif(n_pts, min=x_min, max=x_max)

param_intercept=2.3
param_slope=0.67
y_pred=param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main="Simulated Data \nNo Errors",
    xlab="", ylab="")

#Normal Errors 1
error_mean=0
error_sd=0.25

y_observed= y_pred + rnorm(
  n=n_pts, mean=error_mean, sd=error_sd)
plot(x_sim, y_observed,
     main="Normally Distributed Errors \nConstant Variance",
     xlab="", ylab="")

#Normal Errors 2
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, 
     main = "Normally Distributed Errors\n Constant Variance", 
     xlab = "", ylab = "")
plot(x_sim, y_observed_2, 
     main = "Normally Distributed Errors\n Increasing Variance",
     xlab = "", ylab = "")

#Call in the data file
require("here")
dat_dispersal= data.frame(read.csv(here("data", "dispersal.csv")))

#Plot the First time breeder dispersal rate vs the distance class
plot(dat_dispersal$disp.rate.ftb, dat_dispersal$dist.class)

#Create line_point_slope function
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

#Plot the linear model curve onto the scatterplot
curve(line_point_slope(x, 0.2, 800, -1000), add=TRUE)


#Create the exponential function
exp_fun = function(x, a, b)
{
  return(a*exp(-b*x))
}

#Plot the exponential model curve, pre-questions 
curve(
  exp_fun(x, 1500, 2),
  from=0, to=1, add=TRUE,
  main="Exponential function",
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


#QUESTION 1
exp_fun = function(x, a, b)
{
  return(a*exp(-b*x))
}

#QUESTION 2
curve(exp_fun(x, 1.9, 0.1), add=TRUE,
        from=0, to=20, ylim=c(0,2), ann=FALSE,
        axes=TRUE, type="l", ylab = "f(x)",
        col="black",  lty="solid"); box()


curve(exp_fun(x, 1.9, 0.3), add=TRUE,
              type="l", ylab = "f(x)",
              col="black",  lty="dotted" )

curve(exp_fun(x, 1.2, 0.2), add=TRUE,
       type="l", ylab = "f(x)",
       col="red",  lty="solid" )

curve(exp_fun(x, 1.2, 0.4), add=TRUE,
      type="l", ylab = "f(x)",
      col="red",  lty="dotted" )

#RICKER FUNCTION STUFF
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

#QUESTION 5
curve(
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 75, add = TRUE,
  ylab = "f(x)", xlab = "x",type="l",
  col="black", lty="solid")

curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 75, add = TRUE,
  ylab = "f(x)", xlab = "x",type="l",
  col="black", lty="dotted")

curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 75, add = TRUE,
  ylab = "f(x)", xlab = "x",type="l",
  col="blue", lty="dotted")

curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 75, add = TRUE,
  ylab = "f(x)", xlab = "x",type="l",
  col="red", lty="solid")

curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 75, add = TRUE,
  ylab = "f(x)", xlab = "x",type="l",
  col="red", lty="dotted")

curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 75, add = TRUE,
  ylab = "f(x)", xlab = "x",type="l",
  col="green", lty="dotted")

#QUESTION 8
locator(1)

plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb,
     ylab="Dispersal Rate", xlab="Distance Class",
     main="Vineyard Model")

#Linear Model
curve(line_point_slope(x, 800, 0.3, -0.0008), add=TRUE)

#Exponential Model
curve(
  exp_fun(x, 0.8, 0.002), add=TRUE,
  axes=TRUE, ylim=c(0,2), from=0, to=1800, type="l",
  ann = FALSE, ylab = "f(x)"); box()

#Ricker Model
curve(
  ricker_fun(x, 0.0054, 0.0025), 
  from = 0, to = 1800, add = TRUE,
  ylab = "f(x)", xlab = "x")


#QUESTION 14
#EXAMPLE: y_predicted_vec=line_point_slope(dat$x, guess_x, guess_y, guess_slope)

linear_predicted_vec= line_point_slope(dat_dispersal$dist.class, 800, 0.3, -0.0008)
resids_linear= linear_predicted_vec - dat_dispersal$disp.rate.ftb

exp_predicted_vec= exp_fun(dat_dispersal$dist.class, 0.8, 0.002)
resids_exp= exp_predicted_vec - dat_dispersal$disp.rate.ftb

ricker_predicted_vec=ricker_fun(dat_dispersal$dist.class, 0.0054, 0.0025)
resids_ricker= ricker_predicted_vec - dat_dispersal$disp.rate.ftb

resids= data.frame(resids_linear, resids_exp, resids_ricker)
resids


#QUESTION 15
par(mfrow=c(1,3))

hist(resids_linear, main="Linear Residuals", xlab="Residuals")
hist(resids_exp, main="Exponential Residuals", xlab="Residuals")
hist(resids_ricker, main="Ricker Residuals", xlab="Residuals")

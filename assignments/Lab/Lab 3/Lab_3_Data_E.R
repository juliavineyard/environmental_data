https://michaelfrancenelson.github.io/environmental_data/labs/lab_03_2021.html


install.packages("psych")
require(psych)
pairs.panels(iris)

require("here")
dat_bird = data.frame(read.csv(here("data", "bird.sta.csv")))
head(dat_bird)
str(dat_bird)

dat_habitat = data.frame(read.csv(here("data", "hab.sta.csv")))
head(dat_habitat)

dat_all = merge(dat_bird, dat_habitat)
plot(ba.tot ~ elev, data=dat_all)

dat_bird$CEWA
sum(dat_bird$CEWA)

#Presence/Absence
cewa_present_absent = as.numeric(dat_all$CEWA >= 1)
cewa_present_absent
plot(x=dat_all$elev, y=cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

#Testing different slopes
plot(x = dat_all$elev, y = cewa_present_absent, 
     xlab= "Elevation", ylab="CEWA Presence/Absence",
     cex=2, pch=4, col="forest green")
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

#Question 1 & 2
require(psych)
pairs.panels(data.frame(dat_habitat$elev, dat_habitat$slope,
             dat_habitat$aspect, dat_habitat$ba.tot))

#Question 3 & 4
dat_all$NOFL
nofl_present_absent = as.numeric(dat_all$NOFL >= 1)
nofl_present_absent
plot(x=dat_all$ba.tot, y=nofl_present_absent,
     xlab="Total Basal Area", ylab="NOFL Present/Absent",
     main="Northern Flicker Presence",
     col="blue", cex=2, pch=1)

curve(logistic_midpoint_slope(x, midpoint = 18, slope = -1),
      add = TRUE)
median(dat_all$ba.tot)
mean(dat_all$ba.tot)

#Question 5 & 6
dat_all$DOWO
dowo_present_absent = as.numeric(dat_all$DOWO >= 1)
dowo_present_absent
plot(x=dat_all$ba.tot, y=dowo_present_absent,
     xlab="Total Basal Area", ylab="DOWO Present/Absent",
     main="Downy Woodpecker Presence",
     col="dark green", cex=2, pch=5)

curve(logistic_midpoint_slope(x, midpoint = 18, slope = -1),
      add = TRUE)

#Question 7
sum(dat_all$GRJA)

#Question 9
GRJA_vec = dat_all$GRJA
sum(GRJA_vec >= 1)

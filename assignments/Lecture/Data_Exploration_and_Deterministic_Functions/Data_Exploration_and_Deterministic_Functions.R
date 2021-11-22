install.packages("here")
require("here")
dat_habitat = data.frame(read.csv(here("data", "hab.sta.csv")))
dat_habitat

str(dat_habitat)

#Create histograms of the terrain variables (SIMPLE)
par(mfrow = c(1,3))
hist(dat_habitat$elev, main="Elevation", xlab="Elevation and Frequency")
hist(dat_habitat$aspect, breaks=4,  main="Aspect", xlab="Aspect and Frequency")
hist(dat_habitat$slope, main="Slope", xlab="Slope and Frequency")

mean(dat_habitat$slope)
median(dat_habitat$slope)

summary(dat_habitat$aspect)

#Create scatterplots of total basal area and each terrain variable
#Basic function is plot(x,y)
{
par(mfrow=c(1,3))
plot(dat_habitat$elev, dat_habitat$ba.tot, 
     xlab="Elevation", ylab="Total Basal Area", 
     cex=2, col="sky blue")
plot(dat_habitat$aspect, dat_habitat$ba.tot,
     xlab = "Aspect", ylab="Total Basal Area", 
     cex=2, col="blue")
plot(dat_habitat$slope, dat_habitat$ba.tot,
     xlab = "Slope", ylab="Total Basal Area", 
     cex=2, col="dark blue")
}


#Putting all the plots in 1 figure with titles and colors
{
par(mfrow=c(2,3))
hist(dat_habitat$elev, main="Elevation and Frequency", xlab="Elevation",
     col="sky blue")
hist(dat_habitat$aspect, breaks=4,  main="Aspect and Frequency", xlab="Aspect",
     col="blue")
hist(dat_habitat$slope, main="Slope and Frequency", xlab="Slope",
     col="dark blue")
plot(dat_habitat$elev, dat_habitat$ba.tot, main="Elevation and Basal Area",
     xlab="Elevation", ylab="Total Basal Area",
     cex=2, col="sky blue")
plot(dat_habitat$aspect, dat_habitat$ba.tot, main="Aspect and Basal Area",
     xlab = "Aspect", ylab="Total Basal Area", 
     cex=2, col="blue")
plot(dat_habitat$slope, dat_habitat$ba.tot, main="Slope and Basal Area",
     xlab = "Slope", ylab="Total Basal Area", 
     cex=2, col="dark blue")
}

####THIS IS WHERE I'M TRYING TO FIGURE OUT SLOPE####
#Fitting linear functions to scatterplots
plot(x=dat_habitat$elev, y=dat_habitat$ba.tot, 
     main="Elevation and Basal Area",
     xlab="Elevation", ylab="Total Basal Area",
     cex=2, col="sky blue")
median(dat_habitat$elev)
median(dat_habitat$ba.tot)
data_center_elev=382
data_center_ba.tot=18

#Not sure what this is, he had it in the walk through so I copied it in
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

points(x=data_center_elev, y=data_center_ba.tot, col="red")
curve(line_point_slope(x, data_center_elev, data_center_ba.tot,
                       .3), add=TRUE)
#I don't understand how I'm supposed to visually put a slope on this graph.
#Is my data in wrong?? >NOPE


#Jessica did a line of best fit rather than visually putting a linear slope on
par(mfrow=c(2,3))
plot(x=dat_habitat$elev, y=dat_habitat$ba.tot, 
     main="Elevation and Basal Area",
     xlab="Elevation", ylab="Total Basal Area",
     cex=2, col="sky blue")
abline(lm(dat_habitat$ba.tot~ dat_habitat$elev), col="red")

plot(dat_habitat$aspect, dat_habitat$ba.tot,
     main = "Aspect and Basal Area",
     xlab = "Aspect", ylab="Total Basal Area", 
     cex=2, col="blue")
abline(lm(dat_habitat$ba.tot~ dat_habitat$aspect), col="red")

plot(dat_habitat$slope, dat_habitat$ba.tot,
     main="Slope and Basal Area",
     xlab = "Slope", ylab="Total Basal Area", 
     cex=2, col="dark blue")
abline(lm(dat_habitat$ba.tot~ dat_habitat$slope), col="red")

hist(dat_habitat$elev, main="Elevation and Frequency", xlab="Elevation",
     col="sky blue")
hist(dat_habitat$aspect, breaks=4,  main="Aspect and Frequency", xlab="Aspect",
     col="blue")
hist(dat_habitat$slope, main="Slope and Frequency", xlab="Slope",
     col="dark blue")


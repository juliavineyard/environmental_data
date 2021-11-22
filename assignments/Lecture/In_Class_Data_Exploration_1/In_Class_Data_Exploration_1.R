#Pull in the packages that are needed
require(here)
require(psych)

#Load the data into dataframes from the data files
dat_bird=
  data.frame(read.csv(here("data", "bird.sta.csv")))

dat_hab=
  data.frame(read.csv(here("data", "hab.sta.csv")))

#Check the struture of the columns in dat_hab to know
#which ones are possible to pull into a pairplot
str(dat_hab)

#Create a pair plot of ba.snag, slope, and elev
pairs.panels(data.frame(dat_hab$"ba.snag", 
                        dat_hab$"slope", 
                        dat_hab$"elev"))

#Create a histogram of species specific bird counts
#breaks= cleans up how the data is viewed
hist(dat_bird$CBCH, breaks=0:7-0.5)

hist(dat_bird$BHGR, breaks=0:3)


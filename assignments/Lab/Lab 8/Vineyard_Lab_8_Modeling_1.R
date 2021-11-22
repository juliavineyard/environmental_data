require(palmerpenguins)
penguin_dat= droplevels(subset(penguins, species!="Gentoo"))

#Parametric Two-Sample Test----
t.test(flipper_length_mm ~ species, data=penguin_dat, alternative="less")

#Bootstrap Two-sample Test----
install.packages("simpleboot")
??two.boot

require(simpleboot)

penguin_dat_ade= droplevels(subset(penguins, species=="Adelie"))
penguin_dat_chin= droplevels(subset(penguins, species=="Chinstrap"))
two.boot(penguin_dat_ade$flipper_length_mm, penguin_dat_chin$flipper_length_mm,
         FUN= mean, R=1000)

hist(two.boot(penguin_dat_ade$flipper_length_mm, penguin_dat_chin$flipper_length_mm,
              FUN= mean, R=1000))

#Tree data
require(here)
veg= read.csv(here("data", "vegdata.csv"))

boxplot(pine ~ treatment, data = veg)

#Tree Treatments
dat_tree= droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, data=dat_tree)
table(dat_tree$treatment)

#Nonparametric two-sample test 
aggregate(pine ~ treatment, data=dat_tree, FUN=mean, na.rm=TRUE)
wilcox.test(17.875, 1.875, alternative= "two.sided") #THIS one acutally doesn't tell us anything since it's only comparing 2 numbers
wilcox.test(pine ~ treatment, data=dat_tree) #This is the correct one
  #p-value of 1 means that we fail to reject that both

#Bootstrap
require(boot)
tree_boot= two.boot(subset(dat_tree, treatment == "clipped")$pine,
                    subset(dat_tree, treatment == "control")$pine,
                    FUN=mean, R=1000, na.rm=TRUE)
boot.ci(tree_boot)

hist(tree_boot$t, main="Bootstrap sampling distribution")
quantile(tree_boot$t, 0.025)

#Bird Data
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data","hab.sub.csv"))

dat_all = merge(dat_bird, dat_habitat, by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])
names(dat_all)
"b.sidi" %in% names(dat_all)

#STANDARDIZE THE B.SIDI COLUMN
  #Calculate the sample mean and sd:
b_sidi_mean=mean(dat_all$b.sidi, na.rm=TRUE)
b_sidi_sd= sd(dat_all$b.sidi, na.rm = TRUE)

  #Use the subset-by-name symbol ($) to create a new 
    #column of z-standardized values
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

#STANDARDIZING THE S.SIDI COLUMN
s_sidi_mean=mean(dat_all$s.sidi, na.rm=TRUE)
s_sidi_sd= sd(dat_all$s.sidi, na.rm = TRUE)

#Use the subset-by-name symbol ($) to create a new 
#column of z-standardized values
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

#Model Variables
plot(b.sidi ~ s.sidi, data=dat_all, main="Simpson's Diversity Indices",
     xlab="Vegetation Cover Diversity", ylab="Bird Diversity")

#Simple Linear Regression
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed= coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#The Slope Coefficient
dat_1 = subset(dat_all, select = c(b.sidi, s.sidi))

#Resampling the data
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = data.frame(b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2])

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#Randomization Loop
m = 10000 
result = numeric(m) 

for(i in 1:m)
{index_1=sample(nrow(dat_1), replace = TRUE) 
  index_2=sample(nrow(dat_1), replace = TRUE)
  dat_resampled_i = data.frame(b.sidi = dat_1$b.sidi[index_1],
                               s.sidi = dat_1$s.sidi[index_2])
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i]=coef(fit_resampled_i)[2]
}
print(result[i])

#The Null Distribution
hist(result, main = "Null Distribution of Regression Slope", 
     xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

#Critical Slope Value
quantile(result, c(.05))



##QUESTIONS##

pen_boot= two.boot(penguin_dat_ade$flipper_length_mm, 
         penguin_dat_chin$flipper_length_mm,
         FUN= mean, R=10000, na.rm=TRUE)

hist(pen_boot$t)

quantile(pen_boot$t, c(0.025, 0.975))
mean(pen_boot$t)
median(pen_boot$t)

#Question 1
sd(pen_boot$t)

#Question 2
hist(pen_boot$t, 
     main = "Adelie and Chinstrap Penguin Flipper Length", 
     xlab="Difference in Means")

#Question 3
quantile(pen_boot$t, c(0.025, 0.975))


#Question 5
help(ecdf)
ecdf(-0.5)

pen_ecdf= ecdf(pen_boot$t)

#Question 6
1- pen_ecdf(-4.5)

#Question 7
pen_ecdf(-8)


#Question 9
veg= read.csv(here("data", "vegdata.csv"))
dat_tree= droplevels(subset(veg, treatment %in% c("control", "clipped")))
wilcox.test(pine ~ treatment, data=dat_tree)

#Question 10
tree_boot= two.boot(subset(dat_tree, treatment == "clipped")$pine,
                    subset(dat_tree, treatment == "control")$pine,
                    FUN=mean, R=1000, na.rm=TRUE)

quantile(tree_boot$t, c(0.025, 0.975))

#Question 11
mean(tree_boot$t)

#Questions 12-17
#STANDARDIZE THE B.SIDI COLUMN
#Calculate the sample mean and sd:
b_sidi_mean=mean(dat_all$b.sidi, na.rm=TRUE)
b_sidi_sd= sd(dat_all$b.sidi, na.rm = TRUE)

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

#Question 12
#STANDARDIZING THE S.SIDI COLUMN
s_sidi_mean=mean(dat_all$s.sidi, na.rm=TRUE)
s_sidi_sd= sd(dat_all$s.sidi, na.rm = TRUE)

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd


#Question 14
m = 10000 
result = numeric(m) 
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data","hab.sub.csv"))
dat_all = merge(dat_bird, dat_habitat, by = c("basin", "sub"))
dat_1 = subset(dat_all, select = c(b.sidi, s.sidi))


for(i in 1:m)
{index_1=sample(nrow(dat_1), replace = TRUE) 
index_2=sample(nrow(dat_1), replace = TRUE)
dat_resampled_i = data.frame(b.sidi = dat_1$b.sidi[index_1],
                             s.sidi = dat_1$s.sidi[index_2])
fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
result[i]=coef(fit_resampled_i)[2]
}
print(result[i])

#Question 15
hist(result, main = "Monte Carlo Simulation", 
     xlab = "Slope Parameter")

crit_slope= quantile(result, c(.05))
abline(v = crit_slope, lty = 2, col = "red", lwd = 2)
abline(v=slope_observed, lty=2, col="blue", lwd=2)

#Question 16
crit_slope

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
slope_observed



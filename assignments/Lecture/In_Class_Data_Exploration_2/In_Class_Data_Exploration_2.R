
install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)

penguins = data.frame(penguins)
class(penguins)
mean(penguins$body_mass_g, na.rm = TRUE)
head(penguins)
```

summary(penguins)

plot(penguins$island, penguins$body_mass_g)
plot(penguins$island, penguins$body_mass_g)

#Could never make a histogram work with this dataset!
hist(penguins$bill_depth_mm, penguins$body_mass_g)

boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

par(mfrow = c(1,2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data=penguins)

coplot(body_mass_g ~ bill_depth_mm | sex, data=penguins)


#Don't know what the heck this does, didn't successfully do anything when I tried
require(here)
png(filename = here("basic_histogram.png"), width=800, height=600)
hist(penguins$body_mass_g)


#DID NOT FINISH THIS EXERCISE#
#Submitted report using other's plots#
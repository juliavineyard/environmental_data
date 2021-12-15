require(here)
ginkgo= read.csv(here("data", "ginkgo_data_2021.csv"))

#Question 1
boxplot(notch_depth ~ seeds_present, data=ginkgo)

#Question 2
#there is no difference between seed bearing and non seed bearing trees

#Question 3
plot(max_width ~ max_depth, data=ginkgo,
     xlab= "Max Depth", ylab= "Max Width")

#Question 4
#the max depth is clustered in the 40-60 mm range, with width clustering from 60-100 mm

#Question 5

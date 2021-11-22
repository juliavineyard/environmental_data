prnt_msg=function(msg)
{print(msg)}

x=2

prnt_msg=function(msg)
{print(x)}



require(palmerpenguins)
require(here)

image_file="ugly_histogram.png"

png(here("images", image_file), 
    width = 1200, height = 1000)


hist(penguins$flipper_length_mm)

dev.off()

save_png_1=function(image_file)
{
  require(here)
  png(here("images", image_file), 
     width = 1200, height = 1000)
  }
  
save_png_1("ugly_histo_2.png")
hist(penguins$body_mass_g)
dev.off()
  
dat_vec=penguins$body_mass_g
my_title = "Julia's Histogram!"
x_label= "Julia's Data!"

hist(dat_vec, col="steelblue",
     main=my_title, 
     xlab= x_label)


#Creating steelblue function
steelblue_hist_fun = function(dat_vec, my_title, x_label)
{
  hist(dat_vec, col="steelblue",
       main=my_title, 
       xlab= x_label)
}


#Testing steelblue function
steelblue_hist_fun(
dat_vec=sample(x=1:100, size=1000, replace=TRUE),
my_title = "Julia's Random Numbers",
x_label="x=values")

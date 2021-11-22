require(here)
here("data","catrate.csv")
read.csv("/Users/count/OneDrive/Documents/UMass/Classes
     /Fall 2021/environmental_data")
dat_catrate = 
  data.frame(read.csv(here("data","catrate.csv")))
head(dat_catrate)
str(dat_catrate)
hist(dat_catrate$cat.rate)

dat_delomys=
  data.frame(read.csv(here("data", "delomys.csv")))
head(dat_delomys)
str(dat_delomys)
plot(dat_delomys$body_mass, dat_delomys$body_length,
     main="Body Mass & Body Length\nBy Julia Vineyard",
     xlab="Body Mass", ylab="Body Length")

dat_rope=
  data.frame(read.csv(here("data", "rope.csv")))
head(dat_rope)
str(dat_rope)
plot(dat_rope$p.cut, dat_rope$p.strength,
     main= "P.Cut and P.Strength\nBy Julia Vineyard",
     xlab="P.Cut", ylab="P.Strength")


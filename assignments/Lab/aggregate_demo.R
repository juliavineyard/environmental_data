##AGGREGATE DEMO
10/27/2021 In-Lab

require(palmerpenguins)

#take flipper length, seperate it out by species, calculate mean for each
aggregate(penguins$flipper_length_mm, 
          list(penguins$species), 
          FUN= mean, na.rm=TRUE)

#formula notation doing the exact same thing as above
  #flipper length as explained by species
aggregate(flipper_length_mm ~ species,
          data= penguins, 
          FUN=mean, na.rm=TRUE)

#boxplot with formula notation
boxplot(flipper_length_mm ~ species,
        data= penguins)


#aggregate by more than 1 factor (species and sex)
aggregate(flipper_length_mm ~ species + sex,
          data= penguins,
          FUN=mean, na.rm=TRUE)

#boxplot with 2 conditions
boxplot(flipper_length_mm ~ species + sex,
        data= penguins,
        FUN=mean, na.rm=TRUE)

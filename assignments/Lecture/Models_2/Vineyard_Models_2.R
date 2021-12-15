require(palmerpenguins)

#1-sample t-test ----
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

t.test(x=subset(penguins, species == "Gentoo")$flipper_length_mm, mu=218)

t.test(x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218, alternative = "less")

#2-sample t-test ----
t.test(flipper_length_mm ~ species, 
       data = subset(penguins, species != "Chinstrap"))

#1-Way ANOVA ----
  #Graphical Data Exploration
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", 
     xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), 
     main = "density plot of body mass")

boxplot(body_mass_g ~ species, data = penguins)

  #Numerical Data Exploration
dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)
shapiro.test(dat_chinstrap$body_mass_g)
  #Shapiro test Null= the data are drawn from a normally-distributed population

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)
aggregate(body_mass_g ~ species, data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)

  #Fit a linear model
fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

  #Conduct the ANOVA
anova(fit_species)

#2-way additive model
boxplot(body_mass_g ~ species + sex, data = penguins)
fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

#2-way Interactive ANOVA
fit_interactive= lm(body_mass_g ~ sex * species, data=penguins)
summary(fit_interactive)
anova(fit_interactive)

#QUESTIONS ----
#1
boxplot(formula= body_mass_g ~ species * sex, data=penguins,
        rnorm(100) ~ rbinom(100, 1, 0.5), 
        main = "Penguin Body Mass by Species and Sex",
        names=c("Adelie\n Female", "Chinstrap\n Female", "Gentoo\n Female",
                "Adelie\n Male", "Chinstrap\n Male", "Gentoo\n Male"),
        xlab="Species\n Sex", ylab="Body Mass (g)", col="skyblue")
#4
fit_both= lm(formula= body_mass_g ~ sex * species, data=penguins)
summary(fit_both)

#7
3368.84 + 158.37

#8
aggregate(body_mass_g ~ species * sex, data=penguins, FUN=mean)

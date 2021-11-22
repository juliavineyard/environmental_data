require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)


#Binomial Test for Proportions ----
#how likely is a response of 33/61 if reproductive success and failure
  #are equally likely
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

#what is the evidence that reproductive success is more or less frequent 
  #than the normal-filling rate?
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

binom.test(n_success, n_years, p = normal_fill_rate) #defaulted two-sided

#observed success rate is less than the pond normal-filling rate
binom.test(n_success, n_years, p=normal_fill_rate, alternative="less")

#Comparing 2 variances ----
  #F-statistic represents ratio between 2 variances
    #if variance of 2 samples is the same then the ratio of variances = 1

veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)

#Variance Test----
#test variance in pine seedling count differs between 2 treatments
var.test(pine ~ treatment, data=veg, 
         subset=treatment %in% c('control', 'clipped'))

shapiro.test(veg$pine[veg$treatment=="control"]) #normally-distributed
shapiro.test(veg$pine[veg$treatment=="clipped"]) #not normally-distributed

#Non-parametric Variance Test----
#null: variances in each of the groups are the same
fligner.test(pine ~ treatment, data=veg,
             subset=treatment %in% c('control','clipped')) #reject the null

#test for homogeneity of variances among all 4 treatments
#Bartlett is parametric
bartlett.test(pine ~ treatment, data=veg) #reject the null

#Fligner is non-parametric
fligner.test(pine ~ treatment, data = veg)#reject the null.

#If you have a parametric test and have a non-normally distributed 
  #data set, the parametric test is going to be more sensitive to that. 
  #So you will have a more extreme p-value

#Comparing 2 Sample Means----
#t-test
t.test(pine ~ treatment, data = veg,
       subset = treatment %in% c('control','clipped')) #cannot reject null

wilcox.test(pine~treatment, data=veg, 
            subset= treatment %in% c('control', 'clipped')) #cannot reject null

#Tests for Paired Samples ----
control= veg$pine[veg$treatment == 'control']
clipped= veg$pine[veg$treatment == 'clipped']

t.test(clipped, control, paired=TRUE)
t.test(clipped, control)

wilcox.test(clipped, control, paired=TRUE)

#Correlation ----
disp = read.csv(here("data", "dispersal.csv"))
disp
plot(disp$disp.rate.ftb, disp$disp.rate.eb)

cor.test(disp$disp.rate.ftb, disp$disp.rate.eb,
         use='complete.obs')

cor.test(disp$disp.rate.ftb, disp$disp.rate.eb,
         use='complete.obs', method='spearman')

#Comparing two distributions ----
plot(ecdf(disp$disp.rate.ftb), verticals=TRUE)

plot(ecdf(disp$disp.rate.eb), verticals=TRUE,
  lty=3,add=TRUE)

ks.test(disp$disp.rate.ftb, disp$disp.rate.eb)
#The null hypothesis is that both groups were sampled from populations 
  #with identical distributions 
 #not enough evidence to suggest that the dispersal-distance relationship
    #differs between first-time breeders and experienced breeders

#Comparing two or more proportions----
  #binomial proportions test, specify 2 vectors: number of mortalities for
    #each sex and total number of each sex
prop.test(c(4,16), c(40,250))
  #insignificant p-value means that there is insufficient evidence to reject the null
    #and we would conclude that the proportions are not statistically difference
    #This test indicates that the observed difference in male and female road
      #mortality is merely a sampling artifact

#Dependence of variables in a contingency table----
#contingency tables- counts are cross-classified according to one or more 
  #categorical contingent variables; shows the counts of how many times each of
  #the possible events of interest actually happened in a particular sample


owls= matrix(c(16, 9, 4, 11), nrow=2)
rownames (owls)= c("present", "absent")
colnames (owls)= c("old", "young")
chisq.test(owls)

fisher.test(owls)

birds= read.csv(here("data", "bird.sta.csv"))
hab= read.csv(here("data", "hab.sta.csv"))
birdhab= merge(birds, hab, by=c("basin", "sub", "sta"))

#Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

#set the presence to be in the first column
br_creeper_table= table(birdhab$s.edge, birdhab$BRCR >0)[,2:1]
br_creeper_table

x= chisq.test(br_creeper_table)
x$expected
x

###QUESTIONS### ----
#Q2
br_creeper_table= table(birdhab$s.edge, birdhab$BRCR >0)[,2:1]
br_creeper_table

x= chisq.test(br_creeper_table)
x$expected
x

#Q3
require(palmerpenguins)
fit_fl_sp= lm(formula= flipper_length_mm ~ species, data= penguins)
fit_fl_sp

fit_species= lm(formula=body_mass_g ~ species, data=penguins)
fit_species

#Q4
fit_sex= lm(formula=body_mass_g ~ sex, data=penguins)
fit_sex

#Q5
fit_both= lm(formula= body_mass_g ~ species * sex, data=penguins)
fit_both

#Q6
boxplot(formula=body_mass_g ~ species, data=penguins,
        rnorm(100) ~ rbinom(100, 1, 0.5), 
        main = "Penguin Body Mass by Species", 
        names = c("Adelie", "Chinstrap", "Gentoo"),
        xlab="Species", ylab="Body Mass (g)", col="skyblue")

#Q7
boxplot(formula=body_mass_g ~ sex, data=penguins,
        rnorm(100) ~ rbinom(100, 1, 0.5), 
        main = "Penguin Body Mass by Sex",
        names=c("Female", "Male"),
        xlab="Sex", ylab="Body Mass (g)", col="skyblue")

#Q8
boxplot(formula= body_mass_g ~ species * sex, data=penguins,
        rnorm(100) ~ rbinom(100, 1, 0.5), 
        main = "Penguin Body Mass by Species and Sex",
        names=c("Adelie\n Female", "Chinstrap\n Female", "Gentoo\n Female",
                "Adelie\n Male", "Chinstrap\n Male", "Gentoo\n Male"),
        xlab="Species\n Sex", ylab="Body Mass (g)", col="skyblue")

#Q11
bartlett.test(formula= body_mass_g ~ species, data=penguins)

#Q12
bartlett.test(formula= body_mass_g ~ sex, data=penguins)

#Q13
dat_spec_sex= aggregate(formula= body_mass_g ~ species * sex, data=penguins,
                        FUN=c)
bartlett.test(dat_spec_sex$body_mass_g)


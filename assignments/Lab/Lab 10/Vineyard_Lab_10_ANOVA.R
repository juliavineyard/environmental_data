require(here)
rope= read.csv(here("data", "rope.csv"))

rm(list = ls())

rope = read.csv(here("data", "rope.csv"))
rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)
       
n_obs = nrow(rope)
n_obs
n_groups = length(levels(rope$rope.type))
n_groups
                        
mean_cut= mean(rope$p.cut)
resids_cut= rope$p.cut - mean_cut
ss_tot= sum(resids_cut^2)
ss_tot
df_tot = n_obs -1
df_tot                

agg_resids= aggregate(x=rope$p.cut, by=list(rope$rope.type),
          FUN= function(x) {x-mean(x)})
str(agg_resids)

agg_sq_resids = aggregate(x=rope$p.cut, by=list(rope$rope.type),
                          FUN= function(x) {sum((x-mean(x))^2)})
agg_sq_resids
str(agg_sq_resids)
ss_within = sum(agg_sq_resids$x)
ss_within
df_within = n_obs - n_groups
df_within
                        
ss_among = ss_tot - ss_within
ss_among
df_among = n_groups - 1
df_among
                        
ms_within =ss_within / df_within
ms_among  =ss_among / df_among
                        
f_ratio = ms_among / ms_within
f_ratio #if a big number then reject the null that all means are the same

f_pval = pf(f_ratio, df_among, df_within, lower.tail= FALSE)
f_pval

fit_1= lm(p.cut ~ rope.type, data=rope)
anova_fit_1 =anova(fit_1)
str(anova_fit_1)

anova_fit_1$`Sum Sq`

#SELF CHECK ----
# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)


#QUESTIONS ----
#3
bartlett.test(p.cut ~ rope.type, data= rope)

#5
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

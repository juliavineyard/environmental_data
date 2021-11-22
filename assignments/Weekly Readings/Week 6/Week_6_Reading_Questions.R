rm(list = ls())

pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = (pol_n_predation + pol_n_no_predation)
pol_predation_rate = (pol_n_predation/pol_n_total)
  
psd_n_predation = 25
psd_n_no_predation = 706
psd_n_total = (psd_n_predation + psd_n_no_predation)
psd_predation_rate = (psd_n_predation/psd_n_total)

print(
  paste0("The seed predation rate for Polysycias fulva is: ", 
             round(pol_predation_rate, digits=3)))

print(
  paste0("The seed predation rate for Pseudospondias microcarpa is ", 
             round(psd_predation_rate, digits=3)))

seeds=matrix(c(round(pol_n_predation, digits=3), round(pol_n_no_predation, digits=3), 
              round(pol_n_total, digits=3), round(pol_predation_rate, digits=3),
              round(psd_n_predation, digits=3), round(psd_n_no_predation, digits=3),
              round(psd_n_total, digits=3), round(psd_predation_rate, digits=3)), 
            ncol=2, nrow=4, byrow=FALSE)
colnames(seeds)= c("Polyscias fulva (pol)", "Pseudospondias microcarpa (psd)")
rownames(seeds)=c("Any taken", "None taken", "N", "Predation rate")


#Ratio of seed predation proportions
predation_ratio=(pol_predation_rate/psd_predation_rate)
predation_ratio

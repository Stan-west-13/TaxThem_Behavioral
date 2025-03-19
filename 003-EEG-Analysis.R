library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ez)

d <- read.table("data/Pz_Cz_CPz_300_500.txt", header = T) %>%
  pivot_longer(cols = starts_with("bin"),
               names_to = c("bin","electrode"),
               values_to = "mean_amp",
               names_sep = "_") %>%
  mutate(bin = factor(bin, levels = unique(bin), labels = c("Inhib_All",
                                                            "Stand_All",
                                                            "Fill_All",
                                                            "Them_Stand", 
                                                            "Tax_Stand",
                                                            "Them_Inhib", 
                                                            "Tax_Inhib")),
         PPID = str_extract(d$ERPset, "0+[0-9]+[0-9]"),.after = ERPset)


## T-test Related vs unrelated
d_electrode_split <- split(d,d$electrode)

map(d_electrode_split, function(x){
  t.test(x$mean_amp[x$bin == "Stand_All"],x$mean_amp[x$bin == "Fill_All"],paired = T)
})


## ANOVA between Two standard and Two inhibition
map(d_electrode_split, function(x){
  cont <- x %>%
    filter(bin == "Them_Stand"|bin == "Tax_Stand"|bin == "Them_Inhib"|bin == "Tax_Inhib")
  m <- ezANOVA(data = cont,
          wid = PPID,
          within = bin,
          dv = mean_amp)
  t <- pairwise.t.test(cont$mean_amp,cont$bin, paired = T, pool.sd = F,p.adjust.method = "bonferroni")
return(list(ANOVAs = m,t = t))
})




## ANOVA between Two standard and Inhibition all
map(d_electrode_split, function(x){
  cont <- x %>%
    filter(bin == "Them_Stand"|bin == "Tax_Stand"|bin == "Inhib_All")
  m <- ezANOVA(data = cont,
               wid = PPID,
               within = bin,
               dv = mean_amp)
  t <- pairwise.t.test(cont$mean_amp,cont$bin, paired = T, pool.sd = F,p.adjust.method = "bonferroni")
  return(list(ANOVAs = m,t = t))
})



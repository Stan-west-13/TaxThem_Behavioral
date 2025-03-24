library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ez)
library(rstatix)

## Load in data and change names
# condition - is it inhibition, standard, or filler trial
# word-type - is it taxonomic, thematic, or all collapsed?
d <- read.table("data/Pz_Cz_CPz_300_500.txt", header = T) %>%
  pivot_longer(cols = starts_with("bin"),
               names_to = c("bin","electrode"),
               values_to = "mean_amp",
               names_sep = "_") %>%
  mutate(bin = factor(bin, levels = unique(bin), labels = c("All_Inhib",
                                                            "All_Stand",
                                                            "All_Fill",
                                                            "Them_Stand", 
                                                            "Tax_Stand",
                                                            "Them_Inhib", 
                                                            "Tax_Inhib")),
         PPID = str_extract(ERPset, "0+[0-9]+[0-9]"),.after = ERPset) %>%
  pivot_wider(names_from = bin,
              values_from = mean_amp,
              names_sep = "_") %>%
  pivot_longer(cols = starts_with(c("All","Tax","Them")),
               names_to = c("word_type","condition"),
               names_sep = "_",
               values_to = "mean_amp")

### Split data by electrode to use with map(). Run all electrodes at once.
d_electrode_split <- split(d,d$electrode)

## T-test Related vs unrelated
map(d_electrode_split, function(x){
  x <- x %>%
    filter(word_type == "All") %>%
    filter(condition == "Fill" | condition == "Stand")
  
  t.test(x$mean_amp[x$condition == "Fill"], x$mean_amp[x$condition == "Stand"],paired = T)
})


## ANOVA between Two standard and Two inhibition
map(d_electrode_split, function(x){
  cont <- x %>%
    filter(condition == "Inhib"|condition == "Stand") %>%
    filter( word_type == "Tax" | word_type == "Them") %>%
    ungroup()
  m <- ezANOVA(data = cont,
          wid = PPID,
          within = .(word_type,condition),
          dv = mean_amp)
  by_condition <- cont %>%
    group_by(condition) %>%
    pairwise_t_test(mean_amp ~ word_type, paired = T, pool.sd = F,p.adjust.method = "bonferroni")
  by_word_type <- cont %>%
    group_by(word_type) %>%
    pairwise_t_test(mean_amp ~ condition, paired = T, pool.sd = F,p.adjust.method = "bonferroni")
return(list(ANOVAs = m,by_word_type = by_word_type, by_condition = by_condition))
})


## ANOVA between All filler, standard, and inhibition
map(d_electrode_split, function(x){
  cont <- x %>%
    filter(word_type == "All") 
  m <- ezANOVA(data = cont,
               wid = PPID,
               within = condition,
               dv = mean_amp)
  t <- pairwise_t_test(cont,mean_amp~condition, paired = T, pool.sd = F,p.adjust.method = "bonferroni")
  return(list(ANOVAs = m,t = t))
})








library(dplyr)
library(ggplot2)
files_appended <- readRDS("data/files_appended.rds")

files_appended$correct <- ifelse(files_appended$correct == 2, 0, files_appended$correct)

files_appended_factored <- files_appended %>%
  mutate(button = factor(button, levels = c(1,2,3,4), labels = c('p','n','unknown','unknown')),
         word_type = factor(cond, labels =c("filler", "thematic","taxonomic") , levels =c(0,1,2)),
         is_correct = factor(correct,levels = c(0,1) , labels =c(FALSE,TRUE)),
         block = as.factor(ifelse(block == 1 & counterbalance == 1,
                                  "Thematic_inhib", 
                                  ifelse(block == 1 & counterbalance == 2, 
                                         "Taxonomic_inhib",
                                         ifelse(block == 2 & counterbalance == 1,
                                                "Taxonomic_inhib",
                                                ifelse(block == 2 & counterbalance ==2,
                                                       "Thematic_inhib",NA))))),
         counterbalance = factor(counterbalance,levels =c(1,2), labels =c("Them_first", "Tax_first") ),
         PPID = as.factor(subno),
         trial_condition = as.factor(ifelse(cond == 0 & block == "Taxonomic_inhib", 
                                            "FillTax", 
                                            ifelse(cond == 1 & block == "Thematic_inhib", 
                                                   "ThemN",
                                                   ifelse(cond == 1 & block == "Taxonomic_inhib",
                                                          "ThemP",
                                                          ifelse(cond == 2 & block == "Thematic_inhib",
                                                                 "TaxP",
                                                                 ifelse(cond == 2 & block == "Taxonomic_inhib",
                                                                        "TaxN",
                                                                        ifelse(cond == 0 & block == "Thematic_inhib",
                                                                               "FillThem",NA)))))))) %>%
  relocate(PPID, trial_condition,word_type, block, counterbalance, is_correct, button,rt,running_clock = time,order) %>%
  #filter(!button == "unknown") %>%
  select(-cond,-correct,-subno) %>%
  group_by(PPID) %>%
  mutate(accuracy_participant = sum(is_correct == TRUE)/n(),
         mean_rt_participant = mean(rt)) %>%
  group_by(trial_condition) %>%
  mutate(accuracy_trialType = sum(is_correct == TRUE)/n(),
         mean_rt_trialType = mean(rt)) %>%
  group_by(counterbalance,.add = TRUE) %>%
  mutate(accuracy_trialType_counterbalance = sum(is_correct == TRUE)/n()) %>%
  ungroup() %>%
  group_by(PPID,word_type, block) %>%
  mutate(accuracy_wordType_PPID = sum(is_correct == TRUE)/n()) %>%
  ungroup()
  


  
saveRDS(files_appended_factored, file = paste0("data/","logfiles_metadata_",Sys.Date(),".rds"))


plot_df <- files_appended_factored %>%
  select(PPID, 
         word_type,
         block, 
         trial_condition,
         counterbalance,
         starts_with("accuracy"),
         starts_with("mean")) %>%
  unique()

ggplot(plot_df, aes(x = word_type, y = accuracy_wordType_PPID))+
  geom_violin()+
  geom_point()+
  geom_line(aes(group = PPID))+
  facet_wrap(~block)

ggplot(plot_df, aes(x = trial_condition, y = mean_rt_trialType))+
  geom_point()




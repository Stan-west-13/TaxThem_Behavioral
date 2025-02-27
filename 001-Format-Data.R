library(dplyr)
library(ggplot2)
files_appended <- readRDS("data/files_appended.rds")

files_appended$correct <- ifelse(files_appended$correct == 2, 0, 1 )

files_appended_factored <- files_appended %>%
  mutate(button = as.factor(button),
         is_correct = factor(correct,levels = c(0,1) , labels =c(FALSE,TRUE)),
         block = as.factor(ifelse(block == 1 & counterbalance == 1,"Taxonomic_inhib", ifelse(block == 2 & counterbalance == 2, "Taxonomic_inhib","Thematic_inhib"))),
         counterbalance = factor(counterbalance,levels =c(1,2), labels =c("Tax_first", "Them_first") ),
         PPID = as.factor(subno),
         trial_condition = as.factor(ifelse(cond == 0 & block == "Taxonomic_inhib", 
                                            "FillTax", 
                                            ifelse(cond == 1 & block == "Thematic_inhib", 
                                                   "ThemN",
                                                   ifelse(cond == 2 & block == "Taxonomic_inhib",
                                                          "TaxN",
                                                          ifelse(cond == 1 & block == "Taxonomic_inhib",
                                                                 "ThemP",
                                                                 ifelse(cond == 2 & block == "Taxonomic_inhib",
                                                                        "TaxP","FillThem")))))),) %>%
  relocate(PPID, trial_condition, block, counterbalance, is_correct, button,rt,running_clock = time,order) %>%
  select(-cond,-correct,-subno) %>%
  group_by(PPID) %>%
  mutate(accuracy_overall = sum(is_correct == TRUE)/n(),
         mean_rt_overall = mean(rt)) %>%
  group_by(block, .add = TRUE) %>%
  mutate(accuracy_blockwise = sum(is_correct == TRUE)/n(),
         mean_rt_blockwise = mean(rt)) %>%
  group_by(trial_condition, .add = TRUE) %>%
  mutate(accuracy_blockwise_trialType = sum(is_correct == TRUE)/n(),
         mean_rt_blockwise_trialType = mean(rt))
  
saveRDS(files_appended_factored, file = paste0("data/","logfiles_metadata_",Sys.Date(),".rds"))


plot_df <- files_appended_factored %>%
  select(PPID, 
         block, 
         trial_condition,
         counterbalance,
         starts_with("accuracy"),
         starts_with("mean")) %>%
  unique()

ggplot(plot_df, aes(x = trial_condition, y = mean_rt_blockwise_trialType, fill = block))+
  geom_violin()


cond, labels =c("filler", "thematic","taxonomic") , levels =c(0,1,2)
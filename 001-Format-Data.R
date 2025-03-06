library(dplyr)
library(ggplot2)
library(ez)
files_appended <- readRDS("data/files_appended.rds")

files_appended$correct <- ifelse(files_appended$correct == 2, 0, files_appended$correct)


files_appended_factored <- files_appended %>%
  mutate(button = factor(button, levels = c(1,2,3,4), labels = c('p','n','unknown','unknown')),
         word_type = factor(cond, labels =c("filler", "thematic","taxonomic") , levels =c(0,1,2)),
         is_correct = ifelse(correct == 0, FALSE,TRUE),
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
         trial_condition = as.factor(ifelse(word_type == "filler" & block == "Taxonomic_inhib", 
                                            "FillTax", 
                                            ifelse(word_type == "thematic" & block == "Thematic_inhib", 
                                                   "ThemN",
                                                   ifelse(word_type == "thematic" & block == "Taxonomic_inhib",
                                                          "ThemP",
                                                          ifelse(word_type == "taxonomic" & block == "Thematic_inhib",
                                                                 "TaxP",
                                                                 ifelse(word_type == "taxonomic" & block == "Taxonomic_inhib",
                                                                        "TaxN",
                                                                        ifelse(word_type == "filler" & block == "Thematic_inhib",
                                                                               "FillThem",NA)))))))) %>%
  relocate(PPID, trial_condition,word_type, block, counterbalance, is_correct, button,rt,running_clock = time,order) %>%
  #filter(!button == "unknown") %>%
  select(-cond,-correct,-subno)
  


  
#saveRDS(files_appended_factored, file = paste0("data/","logfiles_metadata_",Sys.Date(),".rds"))



summary_stats <- files_appended_factored %>%
  group_by(PPID) %>%
  mutate(accuracy_participant = sum(is_correct == TRUE)/n(),
         mean_rt_participant = mean(rt)) %>%
  group_by(trial_condition, .add = TRUE) %>%
  mutate(accuracy_trialType = sum(is_correct == TRUE)/n(),
         mean_rt_trialType = mean(rt)) %>%
  ungroup() %>%
  group_by(PPID,block,word_type) %>%
  mutate(accuracy_block_wordtype_ppid = sum(is_correct == TRUE)/n()) %>%
  ungroup()



plot_df <- summary_stats %>%
  select(PPID, 
         word_type,
         block, 
         trial_condition,
         counterbalance,
         starts_with("accuracy"),
         starts_with("mean")) %>%
  unique()

ggplot(files_appended_factored, aes(x = is_correct, fill = trial_condition))+
  geom_bar(stat = "count",position = "dodge")

ggplot(plot_df, aes(x = trial_condition, y = accuracy_trialType))+
  geom_point()


ggplot(plot_df %>% 
         mutate(trial_condition = factor(trial_condition, 
                                         levels = c("TaxP","ThemP","TaxN", "ThemN", "FillTax", "FillThem"))), aes(x = trial_condition, y = accuracy_trialType))+
  geom_bar(stat = "summary", fun = "mean") +
  coord_cartesian(ylim = c(0.65,1)) 




ggplot(plot_df, aes(x = trial_condition, y = mean_rt_trialType))+
  geom_point()


ggplot(plot_df %>% 
         mutate(trial_condition = factor(trial_condition, 
                                         levels = c("TaxP","ThemP","TaxN", "ThemN", "FillTax", "FillThem"))), aes(x = trial_condition, y = mean_rt_trialType))+
  geom_bar(stat = "summary", fun = "mean")


anova_df <- summary_stats %>%
  select(PPID, accuracy_block_wordtype_ppid,block,word_type,trial_condition,counterbalance) %>%
  unique() %>%
  mutate(z_acc = (accuracy_block_wordtype_ppid-mean(accuracy_block_wordtype_ppid))/sd(accuracy_block_wordtype_ppid)) %>%
  filter(!z_acc <= -3)

m <- ezANOVA(data = anova_df,
             dv = accuracy_block_wordtype_ppid,
             within = .(block, word_type),
             wid = PPID)
m

anova_df %>%
  group_by(word_type,block) %>%
  summarize(means = mean(accuracy_block_wordtype_ppid))


ggplot(anova_df, aes(x = trial_condition, y = accuracy_block_wordtype_ppid, color = block))+
  geom_point(stat = "summary", fun = "mean")+
  facet_wrap(~counterbalance)










#git fetch in terminal for status of online version then git status
#then git status (branch behind or up to date)

df <- readRDS("data/logfiles_metadata_2025-03-06.rds")
#plot_df has accuracy and rt for analysis

#-----------
#accuracy by trial condition in each block type (taxonomic_inhib and thematic_inhib) 
#-----------
#box plot with accuracy of trial condition by counterbalance (for fun)
ggplot(plot_df, aes(x = trial_condition, y = accuracy_trialType, colour = trial_condition)) + 
  geom_bar(stat = "summary", fun = "mean") + coord_cartesian(ylim = c(.60,1)) + facet_wrap(~counterbalance)

#Is there a significant difference between accuracy trial type means
accuracytT_model <- ezANOVA(data = plot_df,
                            dv = accuracy_trialType,
                            wid = PPID,
                            within = trial_condition,
                            detailed = TRUE)
accuracytT_model
#Yes, (DFd = 105, missing PPID= 1)
#F(5,105)= 10.01, p<0.001, eta squared = 0.209

#pairwise t-test
pairwise.t.test(plot_df$accuracy_trialType, plot_df$trial_condition, paired = TRUE, p.adjust.method = "bonferroni")
pairwise.t.test(plot_df$accuracy_trialType, plot_df$trial_condition, paired = TRUE, p.adjust.method = "holm")
#

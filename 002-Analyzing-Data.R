#git fetch in terminal for status of online version then git status
#then git status (branch behind or up to date)

library(pastecs); library(psych)

df <- readRDS("data/logfiles_metadata_2025-03-06.rds")
#plot_df has accuracy and rt for analysis


#-----------
#accuracy by trial condition in each block type (taxonomic_inhib and thematic_inhib) 
#-----------
summary(plot_df$accuracy_trialType)

#box plot with accuracy of trial condition by counterbalance (for fun)
accuracytT_plot <- ggplot(plot_df, aes(x = trial_condition, y = accuracy_trialType, colour = trial_condition)) + 
  stat_summary(aes(y = accuracy_trialType), fun = "mean", geom = "bar") + 
  coord_cartesian(ylim = c(.60,1)) #+ facet_wrap(~counterbalance)
accuracytT_plot

#Is there a significant difference between accuracy trial type means?
accuracytT_model <- ezANOVA(data = plot_df,
                            dv = accuracy_trialType,
                            wid = PPID,
                            within = trial_condition,
                            detailed = TRUE)
accuracytT_model
#Yes; F(5,105)= 10.01, p<0.001, eta squared = 0.209

#pairwise t-test
pairwise.t.test(plot_df$accuracy_trialType, plot_df$trial_condition, paired = TRUE, p.adjust.method = "bonferroni")
pairwise.t.test(plot_df$accuracy_trialType, plot_df$trial_condition, paired = TRUE, p.adjust.method = "holm")
#FillTax: TaxN= p<.05, TaxP= p<.001, ThemP= p<.001
#FillThem: TaxP= p<.001, ThemP= p<.05
#TaxP: ThemN= p<.05
#Rest: Not significant


#----------
#RT influenced by both relation type (tax/them) and response type (negative/positive)
#----------

#Is there an effect of relation type on RT (e.g. Thematic relation type Dog Leash; Taxonomic relation type Dog Horse)
#So, I'll use "block" as IV and "rt" as DV 

#I'm just looking at the rt on its own with the block for now
by(df$rt, df$block, FUN = describe)
#sd is quite high 

#I want to see a count of rt occurance overall
rtOverall_plot <- ggplot(df, aes(x = rt)) + geom_histogram(binwidth = 25)
rtOverall_plot #+ coord_cartesian(xlim = c(0,5000)) 
summary(df$rt) 
sd(df$rt)
#very large maximums (e.g. 32745 ms) that don't aline with mean
#sd is large as well (but maybe this is normal idk)

#plotting rt by block
relationType_RT_plot <- ggplot(df, aes(x = block, y = rt)) + geom_boxplot()
relationType_RT_plot + coord_cartesian(ylim = c(0,5000))
#lots of variability in rt (may not matter but just wanted too look into it)

#doing pairwise t test (dependent t test) to see if difference in mean is significant
#can't get t.test to work in new update, so using this:
pairwise.t.test(df$rt, df$block, paired = TRUE, p.adjust.method = "bonferroni")
#p>.05, not significant 


#Now, I'm doing the same but this time for effect of response type on RT
#I'm interpreting response type as trial_condition

#Looking at descriptive stats by trial
by(df$rt, df$trial_condition, FUN = describe)

#plotting rt by trial 
responseType_RT_plot <- ggplot(df, aes(x = trial_condition, y = rt)) + geom_boxplot()
responseType_RT_plot + coord_cartesian(ylim = c(0,1500))

#Is there a significant difference in rt means between trial conditions

responseType_RT_model <- ezANOVA(data = df,
                                 dv = rt,
                                 wid = PPID,
                                 within = trial_condition,
                                 detailed = TRUE)
responseType_RT_model
#found not significant

#funnnn for some reason pairwise t test isn't working
temp_rt <- as.factor(df$rt)
pairwise.t.test(temp_rt, df$trial_condition, paired = TRUE)


#OKAY, so now I'm going to do the stats for RT being influenced by both relation type
# and response type using plot_df 



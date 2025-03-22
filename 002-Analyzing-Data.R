#git fetch in terminal for status of online version then git status
#then git status (branch behind or up to date)



library(pastecs); library(psych); library(ez) ; library(tidyverse); library(rstatix)

#metadata 
df <- readRDS("data/logfiles_metadata_2025-03-06.rds")

## Computing participant and trialType-wise RT and accuracy means
summary_stats <- files_appended_factored %>%
  filter(!word_type == "filler") %>%
  group_by(PPID) %>%
  mutate(accuracy_participant = sum(is_correct == TRUE)/n(),
         mean_rt_participant = mean(rt),
         z_rt_pp = (rt - mean_rt_participant)/sd(rt)) %>%
  ungroup() %>%
  filter(rt > 150, z_rt_pp < 3) %>%
  group_by(PPID,trial_condition) %>%
  mutate(accuracy_trialType = sum(is_correct == TRUE)/n(),
         mean_rt_trialType = mean(rt)) %>%
  ungroup() %>%
  group_by(PPID,block,word_type) %>%
  mutate(accuracy_block_wordtype_ppid = sum(is_correct == TRUE)/n(),
         mean_rt_block_wordtype_ppid = mean(rt)) %>%
  ungroup() %>%
  group_by(block,word_type) %>%
  mutate(se_acc = sd(accuracy_block_wordtype_ppid)/sqrt(n()),
         se_rt = sd(mean_rt_block_wordtype_ppid)/sqrt(n()))


## Dataframe for plotting descriptives
plot_df <- summary_stats %>%
  select(PPID, 
         word_type,
         block, 
         trial_condition,
         counterbalance,
         starts_with("accuracy"),
         starts_with("mean"),
         starts_with("se")) %>%
  unique()


#-----------
#accuracy by trial condition in each block type (taxonomic_inhib and thematic_inhib) 
#-----------

# take out filler and re run the tests !!!

summary(plot_df$accuracy_trialType)

#bar graph with accuracy per trial condition 
accuracytT_plot <- ggplot(plot_df, aes(x = trial_condition, y = accuracy_trialType, colour = trial_condition)) + 
  stat_summary(aes(y = accuracy_trialType), fun = "mean", geom = "bar") + 
  coord_cartesian(ylim = c(.60,1)) + 
  labs(xlab = "Trial Condition", ylab = "Accuracy", title = "Accuracy by Trial Condition") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") + 
  facet_wrap()
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

# 1) Predicted accuracy would be lowest on trials with negative response (TaxN in Taxonomic_inhib block)
  # a) 
# 2) Predicted filler trials would be less accurate in Taxonomic_inhib block

#-------------------------------
#Accuracy by block and word type 
#-------------------------------

# 1) Predicted accuracy would be lowest on trials with negative response (TaxN in Taxonomic_inhib block)
# 2) Predicted filler trials would be less accurate in Taxonomic_inhib block

# Main effects of block and word type ; and interaction of block x word type on accuracy
accuracyBlock_Word <- plot_df %>%
  filter(!word_type == "filler") %>%
  ezANOVA(data = .,
          dv = accuracy_block_wordtype_ppid,
          wid = PPID,
          within = .(block, word_type),
          detailed = TRUE)
accuracyBlock_Word

plot_df %>%
  group_by(block) %>%
  filter(!word_type == "filler") %>%
  pairwise_t_test(accuracy_block_wordtype_ppid ~ word_type, paired = TRUE) 

# to look at tibble of interaction block and word type
df %>%
  group_by(word_type, block) %>%
  summarize(accuracy = sum(is_correct)/n())

# box plot looking at word_type wrapped by block for accuracy
plot_df %>%
  filter(!word_type == "filler") %>%
  ggplot() +
  aes(x = word_type, color = block,  
      y = accuracy_block_wordtype_ppid) +
  geom_boxplot()+
  facet_wrap(~block)


m <- mean(plot_df$accuracy_block_wordtype_ppid)
mlim <- m - plot_df$se_acc
mmax <- m + plot_df$se_acc
print(mmax)

#!!!interaction plot for block and word type on accuracy
#add error bars to graphs standard error 
average_interaction_plot <- plot_df %>%
  filter(!word_type == "filler") %>%
  ggplot(aes(x = word_type, color = block, group = block, 
             y = accuracy_block_wordtype_ppid, )) + 
  #geom_errorbar(aes(ymin = mlim, ymax = mmax))+
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, size = .75) +
  scale_fill_manual(labels = c("Thematic", "Taxonomic") ,values = c("#58909d","#2A436E"))+
  scale_color_manual(values = c("#58909d","#2A436E"))+
  theme_bw()+
  labs(x = "Word Pair Type",
       y = "Mean Accuracy",
       color = "Condition")+
  theme(legend.position = c(0.5,0.2,4),
        legend.background = element_rect(colour = 'black', fill = 'grey90', size = 1, linetype='solid'),
        text = element_text(size = 16),
        legend.key.size = unit(0.25,"cm"),
        rect = element_rect(fill= "transparent"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(.70,1)) 
average_interaction_plot
# I can't think of a title except for title = "Interaction of Condition and Word Pair Type on Accuracy"
# but it's too long
# I can only get the fun.data = mean

#----------
#RT influenced by both relation type (tax/them) and response type (negative/positive)
#----------

#I'm just looking at the rt on its own with the word type for now
by(df$rt, df$word_type, FUN = describe, na.rm = TRUE)
by(df$rt, df$word_type, FUN = summary, na.rm = TRUE)
by(df$rt, df$word_type, FUN = sd, na.rm = TRUE)
#sd is quite high and mean explains data very differently from 1Q, median, and 3Q 

#I want to see a count of rt occurance overall
rtOverall_plot <- ggplot(df, aes(x = rt)) + geom_histogram(binwidth = 25)
rtOverall_plot #+ coord_cartesian(xlim = c(0,5000))  

summary(df$rt) 
sd(df$rt)
#very large maximums (e.g. 32745 ms) that don't aline with mean
#sd is large as well (but maybe this is normal idk)

#plotting rt by block
relationType_RT_plot <- ggplot(df, aes(x = block, y = rt)) + geom_boxplot()
relationType_RT_plot #+ coord_cartesian(ylim = c(0,5000))
#lots of variability in rt (may not matter but just wanted too look into it)

#doing pairwise t test (dependent t test) to see if difference in mean is significant
#can't get t.test to work in new update, so using this:
#---pairwise.t.test(df$rt, df$block, paired = TRUE, p.adjust.method = "bonferroni")
#p>.05, not significant 


#Now, I'm doing the same but this time for effect of response type on RT
#I'm interpreting response type as trial_condition

#Looking at descriptive stats by trial
by(df$rt, df$trial_condition, FUN = describe, na.rm = TRUE)
by(df$rt, df$trial_condition, FUN = summary, na.rm = TRUE)
by(df$rt, df$trial_condition, FUN = sd, na.rm = TRUE)
#the raw rt means are very different (larger by ~200 ms) from what is described 
#by the 1Q, median, and 3Q
#sd is very high min=736 max=1418


#plotting rt by trial 
responseType_RT_plot <- ggplot(df, aes(x = trial_condition, y = rt)) + geom_boxplot()
responseType_RT_plot #+ coord_cartesian(ylim = c(0,1500))

#Is there a significant difference in rt means between trial conditions
responseType_RT_model <- ezANOVA(data = df,
                                 dv = rt,
                                 wid = PPID,
                                 within = trial_condition,
                                 detailed = TRUE)
responseType_RT_model
#found not significant

#ANOVA for block word type rt interaction
rt_relation_reponse <- df %>%
  filter(!word_type == "filler") %>%
  ezANOVA(data = .,
          dv = rt,
          wid = PPID,
          within = .(block, word_type),
          detailed = TRUE)
rt_relation_reponse
#!!!interaction plot for this as well

#interaction block and word type on rt 
mean_rt_interaction_plot <- plot_df %>%
  filter(!word_type == "filler") %>%
  ggplot(aes(x = word_type, color = block, group = block, 
             y = mean_rt_block_wordtype_ppid)) + 
  #geom_errorbar(aes(ymin = mlim, ymax = mmax))+
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, size = .75) +
  scale_fill_manual(labels = c("Thematic", "Taxonomic") ,values = c("#58909d","#2A436E"))+
  scale_color_manual(values = c("#58909d","#2A436E"))+
  theme_bw()+
  labs(x = "Word Pair Type",
       y = "Mean Response Time (ms)",
       color = "Condition")+
  theme(legend.position = c(0.5,0.2,4),
        legend.background = element_rect(colour = 'black', fill = 'grey90', size = 1, linetype='solid'),
        text = element_text(size = 16),
        legend.key.size = unit(0.25,"cm"),
        rect = element_rect(fill= "transparent"))+
  theme(plot.title = element_text(hjust = 0.5)) 
  #coord_cartesian(ylim = c()) 
mean_rt_interaction_plot



#interaction plot for rt_relation_response
rt_interaction_plot <- df %>%
  filter(!word_type == "filler") %>%
  ggplot(aes(x = ))
#the group argument tells ggplot from which factor the lines are created

summary_stats %>% 
  group_by(block) %>%
  filter(!word_type == "filler") %>%
  pairwise_t_test(mean_rt_block_wordtype_ppid ~ word_type, paired = TRUE)



#I want to look at descriptive stats for PPIDs, especially 20 and 16
by(df$rt, df$PPID, FUN = describe, na.rm = TRUE)
by(df$rt, df$PPID, FUN = summary, na.rm = TRUE)
by(df$rt, df$PPID, FUN = sd, na.rm = TRUE)

#counting amount of data by trial_condition by PPID
ezDesign(df, trial_condition, PPID)
summary(df$trial_condition)

#------------------------------------------
# Removed outliers and re-running analysis
#------------------------------------------

#participants that are 3 sd above the grand mean rt
#dont group by anything; z score rt and filter for everything 3 z scores below the mean
#data frame, add z score column, then filter anything lower than z <3 
#add box plots for outliers 

# Removing outliers (remove below 150 ms to 3 sd from grand mean RT) and making
#new dataframes for analysis

#look at within scores per participant, remove those outliers
newDf <- df %>%
  filter(rt > 150) %>%
  group_by(PPID) %>%
  mutate(accuracy_PPID = sum((is_correct == TRUE)/n()),
         mean_rt_PPID = mean(rt), 
         sd_rt_PPID = sd(rt),
         zscore_rt_PPID = ((rt - mean_rt_PPID)/sd_rt_PPID)) %>%
  filter(zscore_rt_PPID <= 3) %>%
  group_by(trial_condition, .add = TRUE) %>%
  mutate(accuracy_trialType_new = sum((is_correct == TRUE)/n()),
         mean_rt_trialType_new = mean(rt)) %>%
  ungroup() %>%
  group_by(PPID,block,word_type) %>%
  mutate(accuracy_block_wordtype_ppid_new = sum((is_correct == TRUE)/n()),
         mean_rt_block_wordtype_ppid_new = mean(rt)) %>%
  ungroup() 

# organizing newDf
new_plot_df <- newDf %>%
  select(PPID, 
         word_type,
         block, 
         trial_condition,
         counterbalance,
         rt,
         starts_with("accuracy"),
         starts_with("mean"), 
         starts_with("sd")) %>%
  unique()

#histogram to check for normal distribution  
new_rtOverall_plot <- ggplot(new_plot_df, aes(x = rt)) +geom_histogram(binwidth = 25)
new_rtOverall_plot


# Redoing analysis without outliers

#-----------------------
#Accuracy 
#-----------------------

by(new_plot_df$accuracy_trialType_new, new_plot_df$trial_condition, FUN = describe)

#box plot of accuracy by trial type
accuracyTT_new_bar <- ggplot(new_plot_df, aes(x = trial_condition, y = accuracy_trialType_new, colour = trial_condition)) + 
  stat_summary(aes(y = accuracy_trialType_new), fun = "mean", geom = "bar") + 
  coord_cartesian(ylim = c(.60,1)) + labs(x= "Trial Condition", y= "Accuracy", title = "Accuracy by Trial Condition") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
accuracyTT_new_bar


accuracyTT_new_model <- ezANOVA(data = new_plot_df,
                            dv = accuracy_trialType_new,
                            wid = PPID,
                            within = trial_condition,
                            detailed = TRUE)
accuracyTT_new_model

?pairwise.t.test

pairwise.t.test(new_plot_df$accuracy_trialType_new, new_plot_df$trial_condition, paired = TRUE)


m <- aov(accuracy_trialType_new ~ trial_condition + Error(PPID/trial_condition), data = new_plot_df)
summary(m)



#--------------
# RT
#--------------

by(new_plot_df$rt, new_plot_df$PPID, FUN = describe)

ezDesign(newDf, trial_condition, PPID)
summary(newDf$trial_condition)





















#----word association----
#using cutoff quicker 150 ms and what is 3 sd from the grand mean
# 150 ms and 3022.991 ms
(sd(df$rt)) * 3

#remove participants with 3 sd from average 
#anything ran after group by run within variable
df %>%
  group_by(PPID) %>%
  mutate(mean_PPID = mean(rt), .after=PPID) %>%
  ungroup() %>%
  mutate(mean_grand = mean(rt),
         sd_grand = sd(rt),
         z = (mean_PPID - mean_grand)/(sd_grand)) %>%
  ungroup() %>%
  filter(z < 3) %>%
  view()



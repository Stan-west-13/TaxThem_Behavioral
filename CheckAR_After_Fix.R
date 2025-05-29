library(readxl)
library(dplyr)
library(purrr)
library(tidyverse)
library(ggplot2)

sheets_pre <- excel_sheets("data/master_AR_pre-fix.xlsx")
sheets_post <- excel_sheets("data/master_AR.xlsx")



df_pre <- map_dfr(sheets_pre, function(x){
  x <- read_xlsx("data/master_AR_pre-fix.xlsx", sheet = x) %>%
    mutate(SubID = ifelse(is.na(SubID), SubID[1],SubID[1]))
})

df_post <- map_dfr(sheets_post, function(x){
  x <- read_xlsx("data/master_AR.xlsx", sheet = x) %>%
    mutate(SubID = ifelse(is.na(SubID), SubID[1],SubID[1]))
})



df_pre <- df_pre %>%
  filter(Bin == "Total") %>%
  mutate(time = "pre-fix")
df_post <- df_post %>%
  filter(Bin == "Total") %>%
  mutate(time = "post-fix")
all <- rbind(df_pre,df_post)

all_long <- rbind(df_pre,df_post) %>%
  pivot_wider(id_cols = "SubID",
              names_from = "time",
              values_from = "Total Percent Rejected") %>%
  mutate(diff = post-pre)


ggplot(all,aes(x = SubID,y = `Total Percent Rejected`, fill = time))+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 65,hjust = 1))
ggsave("Figures/AR_Before-After_Fix.png")

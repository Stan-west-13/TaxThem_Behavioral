library(readxl)
library(dplyr)
library(purrr)
d <- read_xlsx("data/master_AR.xlsx")

sheets <- excel_sheets("data/master_AR.xlsx")

df <- map_dfr(sheets, function(x){
  x <- read_xlsx("data/master_AR.xlsx", sheet = x) %>%
    mutate(SubID = ifelse(is.na(SubID), SubID[1],SubID[1]))
})


total_average <- mean(df$`Total Percent Rejected`[df$Bin == "Total"])

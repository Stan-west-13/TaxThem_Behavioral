library(readxl)
library(dplyr)
library(purrr)
d <- read_xlsx("data/master_AR.xlsx")

sheets <- excel_sheets("data/master_AR.xlsx")

df <- map_dfr(sheets, function(x){
  x <- read_xlsx("data/master_AR.xlsx", sheet = x) %>%
    mutate(SubID = ifelse(is.na(SubID), SubID[1],SubID[1]))
})


summ_table <- df %>%
  group_by(Bin) %>%
  reframe(m = mean(`Total Percent Rejected`, na.rm = T),
            sd = sd(`Total Percent Rejected`, na.rm = T),
            range = range(`Total Percent Rejected`, na.rm = T))

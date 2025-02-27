library(dplyr)

files <- list.files("../Intermediate Proj/Process EEG/EEG_ERP_Processing/Logfiles_append/",full.names = T)

files_appended <- data.frame()
saveRDS(files_appended, file = "data/files_appended.rds")
for (file in files){
  tmp <- read.table(file, header = T)
  files_appended <- rbind(tmp,files_appended)
}


files_appended_factored <- files_appended %>%
  mutate(cond = factor(cond, labels = , levels =),
         button = factor(button, levels = , labels = ),
         correct = factor(correct,levels = , labels = ),
         block = as.factor(block),
         counterbalance = factor(counterbalance,levels =, labels = ),
         subno = as.factor(subno))
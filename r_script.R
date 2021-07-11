library(readxl)
library(openxlsx)
source("function/incorporate_log.R")

cleaningLog <- read_excel("input/cleaning_log.xlsx")
raw_data <- read_excel("input/raw_data.xlsx")

incorprated_logs <- incorporate_logs(raw_data,cleaningLog,uuid_col = "_uuid")

cleaned_data <- incorprated_logs$cleaned_df
master_cleaning_log <- incorprated_logs$master_cleaning_log
logs_not_in_rawDf <- incorprated_logs$logs_not_in_rawDF
cleaning_log.applied <- incorprated_logs$cleaning_log.applied
duplicate_log <- incorprated_logs$duplicate_logs

write.xlsx(cleaned_data, "output/clean_data.xlsx")
write.xlsx(master_cleaning_log, "output/master_cleaning_log.xlsx")
write.xlsx(logs_not_in_rawDf, "output/logs_not_in_rawDf.xlsx")
write.xlsx(cleaning_log.applied, "output/cleaning_log.changed.xlsx")
write.xlsx(duplicate_log, "output/duplicate_logs.xlsx")


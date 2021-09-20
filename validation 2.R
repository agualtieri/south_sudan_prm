rm(list = ls())

# lib
library(tidyverse)
library(openxlsx)
library(cleaninginspectoR)
library(cluster)

source("./scripts/check_log.R")
source("./scripts/check_time.R")
source("./scripts/data_falsification.R")


# load files
akobo <- "./inputs/2021_05_data/Copy of Akobo Prm_May _cleaning sheet_AM.xlsm"
kapoeta <- "./inputs/2021_05_data/Copy of REACH_SSD_Kap. Prm cleaning log_May.2021_CK_RD.xlsx"
nyal <- "./inputs/2021_05_data/REACH_SSD_Nyal_total_prm_May2021_RD.xlsx"
yambio <- "./inputs/2021_05_data/Copy of REACH_SSD_Yambio_PRM_Total_May2021_31052021_RD.xlsx"
renk <- "./inputs/2021_05_data/Copy of REACH_SSD_Renk Base_PRM Data_May 2021_RD.xlsx"

# akobo - inputs
akobo_sheets <- openxlsx::getSheetNames(akobo)
akobo_SheetList <- lapply(akobo_sheets, openxlsx::read.xlsx, xlsxFile = akobo)
names(akobo_SheetList) <- akobo_sheets

akobo_clean <- akobo_SheetList[[4]]
akobo_log <- akobo_SheetList[[5]]
akobo_del <- akobo_SheetList[[7]]

# akobo - deletions and log
delc <- semi_join(akobo_del, akobo_clean, "uuid") # ok

akobo_log <- akobo_log %>% filter(question.name %in% names(akobo_clean))

akobo.log.c <- check_log(akobo_clean, akobo_log, old_log_var = "old_value", new_log_var = "new_value") %>%
                mutate(., check = ifelse(new_value == value_extracted, "Log applied correctly", "Please check log")) %>%
                filter(check == "Please check log")

write.xlsx(akobo.log.c, paste0("./outputs/validation_outputs/akobo log check_",Sys.Date(),".xlsx"))

# akobo - issues and time
akobo.i <- inspect_all(akobo_clean) %>% filter(!is.na(index))
write.xlsx(akobo.i, paste0("./outputs/validation_outputs/akobo cleaning issues_",Sys.Date(),".xlsx"))

akobo.t <- check_time(akobo_clean, "2", "60") %>% filter(value < 5)
write.xlsx(akobo.t, paste0("./outputs/validation_outputs/akobo time check_",Sys.Date(),".xlsx"))

# other checks
min(akobo_clean$B.hohh_age)
max(akobo_clean$B.hohh_age)

hh.c <- akobo_clean %>% select(starts_with("C."), -contains("note"), -C.total_males, -C.total_females) %>%
                        mutate(across(where(is.character), as.numeric)) %>%
                        mutate(sum_check = rowSums(.[1:8])) %>%
                        filter(C.total_household != sum_check)


write.xlsx(hh.c, paste0("./outputs/validation_outputs/akobo hh size check_",Sys.Date(),".xlsx"))


## falsification
tool <- read.xlsx("./inputs/REACH_SSD_PRM_V31_December2020.xlsx")
akobo.ss <- calculateDifferences(akobo_clean, tool) %>% filter(number.different.columns < 5)

write.xlsx(akobo.ss, paste0("./outputs/validation_outputs/akobo similar surveys_",Sys.Date(),".xlsx"))



## kapoeta
kapoeta_sheets <- openxlsx::getSheetNames(kapoeta)
kapoeta_SheetList <- lapply(kapoeta_sheets, openxlsx::read.xlsx, xlsxFile = kapoeta)
names(kapoeta_SheetList) <- kapoeta_sheets

kapoeta_clean <- kapoeta_SheetList[[2]]
names(kapoeta_clean)[names(kapoeta_clean) == "X_uuid"] <- "uuid"

kapoeta_log <- kapoeta_SheetList[[3]]

## check issues and time
kapoeta_i <- inspect_all(kapoeta_clean) %>% filter(!is.na(index))
write.xlsx(kapoeta_i, paste0("./outputs/validation_outputs/kapoeta cleaning issues_",Sys.Date(),".xlsx"))

# other checks
min(kapoeta_clean$B.hohh_age)
max(kapoeta_clean$B.hohh_age)

hh.c <- kapoeta_clean %>% select(starts_with("C."), -contains("note"), -C.total_males, -C.total_females) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(sum_check = rowSums(.[1:8])) %>%
  filter(C.total_household != sum_check)


write.xlsx(hh.c, paste0("./outputs/validation_outputs/kapoeta hh size check_",Sys.Date(),".xlsx"))

## check similarity
kapoeta_clean <- kapoeta_clean[!duplicated(kapoeta_clean$uuid), ]
kapoeta_clean <- kapoeta_clean[, !duplicated(colnames(kapoeta_clean))]

kapoeta.ss <- calculateDifferences(kapoeta_clean, tool) %>% filter(number.different.columns < 5)
write.xlsx(kapoeta.ss, paste0("./outputs/validation_outputs/kapoeta similar surveys_",Sys.Date(),".xlsx"))


# nyal
nyal_sheets <- openxlsx::getSheetNames(nyal)
nyal_SheetList <- lapply(nyal_sheets, openxlsx::read.xlsx, xlsxFile = nyal)
names(nyal_SheetList) <- nyal_sheets

nyal_clean <- nyal_SheetList[[2]]
names(nyal_clean)[names(nyal_clean) == "_uuid"] <- "uuid"
nyal_log <- nyal_SheetList[[3]]

## check issues and time
nyal_i <- inspect_all(nyal_clean) %>% filter(!is.na(index))
write.xlsx(nyal_i, paste0("./outputs/validation_outputs/nyal cleaning issues_",Sys.Date(),".xlsx"))

# other checks
min(nyal_clean$B.hohh_age)
max(nyal_clean$B.hohh_age)

hh.c <- nyal_clean %>% select(starts_with("C."), -contains("note"), -C.total_males, -C.total_females) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(sum_check = rowSums(.[1:8])) %>%
  filter(C.total_household != sum_check)

write.xlsx(hh.c, paste0("./outputs/validation_outputs/nyal hh size check_",Sys.Date(),".xlsx"))

## check similarity
nyal.ss <- calculateDifferences(nyal_clean, tool) %>% filter(number.different.columns < 5)
write.xlsx(nyal.ss, paste0("./outputs/validation_outputs/nyal similar surveys_",Sys.Date(),".xlsx"))


# yambio
yambio_sheets <- openxlsx::getSheetNames(yambio)
yambio_SheetList <- lapply(yambio_sheets, openxlsx::read.xlsx, xlsxFile = yambio)
names(yambio_SheetList) <- yambio_sheets

yambio_clean <- yambio_SheetList[[2]]
yambio_log <- yambio_SheetList[[3]]
yambio_del <- yambio_SheetList[[4]]

## check issues and time
yambio_i <- inspect_all(yambio_clean) %>% filter(!is.na(index))
write.xlsx(yambio_i, paste0("./outputs/validation_outputs/yambio cleaning issues_",Sys.Date(),".xlsx"))

# other checks
min(yambio_clean$B.hohh_age)
max(yambio_clean$B.hohh_age)

hh.c <- yambio_clean %>% select(starts_with("C."), -contains("note"), -C.total_males, -C.total_females) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(sum_check = rowSums(.[1:8])) %>%
  filter(C.total_household != sum_check)

write.xlsx(hh.c, paste0("./outputs/validation_outputs/yambio hh size check_",Sys.Date(),".xlsx"))


## check similarity
yambio_clean <- yambio_clean[, !duplicated(colnames(yambio_clean))]

yambio.ss <- calculateDifferences(yambio_clean, tool) %>% filter(number.different.columns < 5)
write.xlsx(yambio.ss, paste0("./outputs/validation_outputs/yambio similar surveys_",Sys.Date(),".xlsx"))


# renk
renk_sheets <- openxlsx::getSheetNames(renk)
renk_SheetList <- lapply(renk_sheets, read.xlsx, xlsxFile = renk)
names(renk_SheetList) <- renk_sheets

renk_clean <- renk_SheetList[[3]]
renk_log <- renk_SheetList[[4]]

## check issues and time
renk_i <- inspect_all(renk_clean) %>% filter(!is.na(index))
write.xlsx(renk_i, paste0("./outputs/validation_outputs/renk cleaning issues_",Sys.Date(),".xlsx"))

# other checks
min(renk_clean$B.hohh_age)
max(renk_clean$B.hohh_age)

hh.c <- renk_clean %>% select(starts_with("C."), -contains("note"), -C.total_males, -C.total_females) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(sum_check = rowSums(.[1:8])) %>%
  filter(C.total_household != sum_check)

write.xlsx(hh.c, paste0("./outputs/validation_outputs/renk hh size check_",Sys.Date(),".xlsx"))

## check similarity
renk_clean <- renk_clean[, !duplicated(colnames(renk_clean))]

renk.ss <- calculateDifferences(renk_clean, tool) %>% filter(number.different.columns < 5)
write.xlsx(renk.ss, paste0("./outputs/validation_outputs/renk similar surveys_",Sys.Date(),".xlsx"))

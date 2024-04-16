rm(list = ls())

#install.packages("readxl")
library("readxl")

library(tidyverse)
library(dplyr)

#install.packages("writexl")
library("writexl")

df <- read_excel("S24Hiring Data.xlsx", col_names = TRUE)

df[df == "997335"] <- "00997335"
df[df == "962040"] <- "00962040"

colnames(df) <- c("GUID", "Interview", "Accepted")

new_df = df[df$Interview != "No", ] 

#interview

interview_df <- new_df %>% select(1, 2)

interview_df$Interview <- gsub("Yes, ","", as.character(interview_df$Interview))
interview_df$Interview <- gsub("Yes; ","", as.character(interview_df$Interview))
interview_df$Interview <- gsub("Yes: ","", as.character(interview_df$Interview))

library(stringr)
interview_df$number.of.comma <- str_count(interview_df$Interview, ",")

#COUNTING NUMBER INTERVIEWED
one_interview <- subset(interview_df, number.of.comma == 0)
two_interview <- subset(interview_df, number.of.comma == 1)
three_interview <- subset(interview_df, number.of.comma == 2)
four_interview <- subset(interview_df, number.of.comma == 3)

#SEPARATE INTO DIFFERENT COLUMNS
two_interview[c('Service 1', 'Service 2')] <- str_split_fixed(two_interview$Interview, ', ', 2)
three_interview[c('Service 1', 'Service 2', 'Service 3')] <- str_split_fixed(three_interview$Interview, ', ', 3)
four_interview[c('Service 1', 'Service 2', 'Service 3', 'Service 4')] <- str_split_fixed(four_interview$Interview, ', ', 4)

#GET RID OF COUNTER COLUMN
one_interview <- one_interview %>% select(1, 2)
colnames(one_interview) <- c("GUID", "Service 1")
one_interview[, 'Service 2'] = NA
one_interview[, 'Service 3'] = NA
one_interview[, 'Service 4'] = NA


two_interview <- two_interview %>% select(1, 4, 5)
two_interview[, 'Service 3'] = NA
two_interview[, 'Service 4'] = NA

three_interview <- three_interview %>% select(1, 4, 5, 6)
three_interview[, 'Service 4'] = NA

four_interview <- four_interview %>% select(1, 4, 5, 6, 7)

interviews_tot <- rbind(one_interview, two_interview, three_interview, four_interview)

GUID_Interview <- interviews_tot$GUID
GUID_Interview <- as.vector(GUID_Interview)

df_interview <- subset(dem_data, dem_data$GUID %in% GUID_Interview)

df_interview <- merge(interviews_tot, dem_data, by = "GUID")

write_xlsx(df_interview, "Interviewed DF.xlsx")



#accepted
new_df <- new_df %>% select(1, 3)
new_df = new_df[new_df$Accepted != "No", ] 

new_df$Accepted <- gsub("Yes, ","", as.character(new_df$Accepted))
new_df$Accepted <- gsub("Yes; ","", as.character(new_df$Accepted))
new_df$Accepted <- gsub("Yes: ","", as.character(new_df$Accepted))


GUID_Accepted <- new_df$GUID
GUID_Accepted <- as.vector(GUID_Accepted)

df_full <- read_excel("Cleaned Applicant Dataset.xlsx")

df_full <- subset(dem_data, dem_data$GUID %in% GUID_Accepted)

df_full <- merge(df_full, new_df, by = "GUID")

write_xlsx(df_full, "Accepted DF.xlsx")

####### fun new ethnicity column for El
dem_data <- read_excel("dem_data_2.xlsx", col_names = TRUE)

#add new columns for interview and accept
dem_data_2 <- dem_data
dem_data_2[, "Interview?"] = "No"
dem_data_2[, "Accepted?"] = "No"

#Interview list
interview_list <- interview_df$GUID

#Accepted list
accepted_list <- new_df$GUID

dem_data_2$`Interview?`[dem_data_2$GUID %in% interview_list] <- "Yes"
dem_data_2$`Accepted?`[dem_data_2$GUID %in% accepted_list] <- "Yes"

write_xlsx(dem_data_2, "DF with Interviewed and Accepted.xlsx")

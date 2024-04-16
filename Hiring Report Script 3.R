rm(list = ls())

#install.packages("readxl")
library("readxl")

library(tidyverse)
library(dplyr)

library("writexl")

df <- read_excel("Applied Services.xlsx", col_names = TRUE)

#COUNTING NUMBER OF SERVICES APPLIED
library(stringr)
df$number.of.comma <- str_count(df$`Services applied to:`, ",")

one_app <- subset(df, number.of.comma == 0)
two_app <- subset(df, number.of.comma == 1)
three_app <- subset(df, number.of.comma == 2)
four_app <- subset(df, number.of.comma == 3)
five_app <- subset(df, number.of.comma == 4)
six_app <- subset(df, number.of.comma == 5)
seven_app <- subset(df, number.of.comma == 6)
eight_app <- subset(df, number.of.comma == 7)
nine_app <- subset(df, number.of.comma == 8)

#SEPARATE INTO DIFFERENT COLUMNS
two_app[c('Service 1', 'Service 2')] <- str_split_fixed(two_app$`Services applied to:`, ', ', 2)
three_app[c('Service 1', 'Service 2', 'Service 3')] <- str_split_fixed(three_app$`Services applied to:`, ', ', 3)
four_app[c('Service 1', 'Service 2', 'Service 3', 'Service 4')] <- str_split_fixed(four_app$`Services applied to:`, ', ', 4)
five_app[c('Service 1', 'Service 2', 'Service 3', 'Service 4', 'Service 5')] <- str_split_fixed(five_app$`Services applied to:`, ', ', 5)
six_app[c('Service 1', 'Service 2', 'Service 3', 'Service 4', 'Service 5', 'Service 6')] <- str_split_fixed(six_app$`Services applied to:`, ', ', 6)
seven_app[c('Service 1', 'Service 2', 'Service 3', 'Service 4', 'Service 5', 'Service 6', 'Service 7')] <- str_split_fixed(seven_app$`Services applied to:`, ', ', 7)
eight_app[c('Service 1', 'Service 2', 'Service 3', 'Service 4', 'Service 5', 'Service 6', 'Service 7', 'Service 8')] <- str_split_fixed(eight_app$`Services applied to:`, ', ', 8)
nine_app[c('Service 1', 'Service 2', 'Service 3', 'Service 4', 'Service 5', 'Service 6', 'Service 7', 'Service 8', 'Service 9')] <- str_split_fixed(nine_app$`Services applied to:`, ', ', 9)

#GET RID OF COUNTER COLUMN
one_app <- one_app %>% select(1, 2)
colnames(one_app) <- c("GUID", "Service 1")
one_app[, 'Service 2'] = NA
one_app[, 'Service 3'] = NA
one_app[, 'Service 4'] = NA
one_app[, 'Service 5'] = NA
one_app[, 'Service 6'] = NA
one_app[, 'Service 7'] = NA
one_app[, 'Service 8'] = NA
one_app[, 'Service 9'] = NA


two_app <- two_app %>% select(1, 4, 5)
two_app[, 'Service 3'] = NA
two_app[, 'Service 4'] = NA
two_app[, 'Service 5'] = NA
two_app[, 'Service 6'] = NA
two_app[, 'Service 7'] = NA
two_app[, 'Service 8'] = NA
two_app[, 'Service 9'] = NA

three_app <- three_app %>% select(1, 4, 5, 6)
three_app[, 'Service 4'] = NA
three_app[, 'Service 5'] = NA
three_app[, 'Service 6'] = NA
three_app[, 'Service 7'] = NA
three_app[, 'Service 8'] = NA
three_app[, 'Service 9'] = NA

four_app <- four_app %>% select(1, 4, 5, 6, 7)
four_app[, 'Service 5'] = NA
four_app[, 'Service 6'] = NA
four_app[, 'Service 7'] = NA
four_app[, 'Service 8'] = NA
four_app[, 'Service 9'] = NA

five_app <- five_app %>% select(1, 4, 5, 6, 7, 8)
five_app[, 'Service 6'] = NA
five_app[, 'Service 7'] = NA
five_app[, 'Service 8'] = NA
five_app[, 'Service 9'] = NA

six_app <- six_app %>% select(1, 4, 5, 6, 7, 8, 9)
six_app[, 'Service 7'] = NA
six_app[, 'Service 8'] = NA
six_app[, 'Service 9'] = NA

seven_app <- seven_app %>% select(1, 4, 5, 6, 7, 8, 9, 10)
seven_app[, 'Service 8'] = NA
seven_app[, 'Service 9'] = NA

eight_app <- eight_app %>% select(1, 4, 5, 6, 7, 8, 9, 10, 11)
eight_app[, 'Service 9'] = NA

nine_app <- nine_app %>% select(1, 4, 5, 6, 7, 8, 9, 10, 11, 12)

apps_tot <- rbind(one_app, two_app, three_app, four_app, five_app, six_app, seven_app, eight_app, nine_app)

######
GUID_Apps <- apps_tot$GUID
GUID_Apps <- as.vector(GUID_Apps)

df <- subset(dem_data, dem_data$GUID %in% GUID_Interview)

dem_data <- read_excel("dem_data_2.xlsx", col_names = TRUE)

df_apps <- subset(dem_data, dem_data$GUID %in% GUID_Apps)
df_apps <- merge(apps_tot, dem_data, by = "GUID")

write_xlsx(df_apps, "Applied DF.xlsx")


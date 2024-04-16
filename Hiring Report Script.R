rm(list = ls())

#install.packages("readxl")
library("readxl")
#install.packages("writexl")
library("writexl")

library(tidyverse)

dem_data <- read_excel("Spring 2023.xlsx")

#drop columns
drop <- c("#", "Full Name", "Net ID", "Georgetown Email")
dem_data = dem_data[,!(names(dem_data) %in% drop)]
dem_data <- dem_data %>% select(1, 2, 6:38)

#rename columns
colnames(dem_data)[19] <- "International Student"
colnames(dem_data)[20] <- "LGBTQ+ Status"
colnames(dem_data)[21] <- "Disability Status"
colnames(dem_data)[22] <- "First Gen Status"
colnames(dem_data)[23] <- "GSP/CSP Status"
colnames(dem_data)[13] <- "Other_Ethnicity"
colnames(dem_data)[14] <- "PNTA_Ethnicity"
colnames(dem_data)[15] <- "Share_Other_Ethnicity"
colnames(dem_data)[17] <- "Share_Other_Gender"
colnames(dem_data)[33] <- "Other_Religion"
colnames(dem_data)[34] <- "PNTA_Religion"
colnames(dem_data)[35] <- "Share_Other_Religion"

#remove other gender
drop <- c("Share_Other_Gender")
dem_data = dem_data[,!(names(dem_data) %in% drop)]

#No response for columns
dem_data$Gender <- replace_na(dem_data$Gender, "No Response")
dem_data$`Military Status` <- replace_na(dem_data$`Military Status`, "No Response")
dem_data$`International Student` <- replace_na(dem_data$`International Student`, "No Response")
dem_data$`LGBTQ+ Status` <- replace_na(dem_data$`LGBTQ+ Status`, "No Response")
dem_data$`Disability Status` <- replace_na(dem_data$`Disability Status`, "No Response")
dem_data$`First Gen Status` <- replace_na(dem_data$`First Gen Status`, "No Response")
dem_data$`GSP/CSP Status` <- replace_na(dem_data$`GSP/CSP Status`, "No Response")

#change 0 and 1s
dem_data$`International Student` <- gsub("0","No", as.character(dem_data$`International Student`))
dem_data$`International Student` <- gsub("1","Yes", as.character(dem_data$`International Student`))

dem_data$`LGBTQ+ Status` <- gsub("0","No", as.character(dem_data$`LGBTQ+ Status`))
dem_data$`LGBTQ+ Status` <- gsub("1","Yes", as.character(dem_data$`LGBTQ+ Status`))

dem_data$`Disability Status` <- gsub("0","No", as.character(dem_data$`Disability Status`))
dem_data$`Disability Status` <- gsub("1","Yes", as.character(dem_data$`Disability Status`))

dem_data$`First Gen Status` <- gsub("0","No", as.character(dem_data$`First Gen Status`))
dem_data$`First Gen Status` <- gsub("1","Yes", as.character(dem_data$`First Gen Status`))

dem_data$`GSP/CSP Status` <- gsub("0","No", as.character(dem_data$`GSP/CSP Status`))
dem_data$`GSP/CSP Status` <- gsub("1","Yes", as.character(dem_data$`GSP/CSP Status`))



#rename ethnicity and religion columns
dem_data$`Black or African American` <- gsub("Black or African American","Black/African_American", as.character(dem_data$`Black or African American`))
dem_data$`Native American/Indigenous` <- gsub("Native American/Indigenous", "Native_American/Indigenous",as.character(dem_data$`Native American/Indigenous`))
dem_data$`Pacific Islander` <- gsub("Pacific Islander", "Pacific_Islander",as.character(dem_data$`Pacific Islander`))
dem_data$`Middle Eastern` <- gsub("Middle Eastern", "Middle_Eastern",as.character(dem_data$`Middle Eastern`))
dem_data$`East Asian` <- gsub("East Asian", "East_Asian",as.character(dem_data$`East Asian`))
dem_data$`Southeast Asian` <- gsub("Southeast Asian", "Southeast_Asian",as.character(dem_data$`Southeast Asian`))
dem_data$`South Asian` <- gsub("South Asian", "South_Asian",as.character(dem_data$`South Asian`))
dem_data$`West Asian` <- gsub("West Asian", "West_Asian",as.character(dem_data$`West Asian`))
dem_data$PNTA_Ethnicity <- gsub("Prefer not to respond", "Prefer_not_to_respond",as.character(dem_data$PNTA_Ethnicity))

#change spaces to _
dem_data$Share_Other_Ethnicity <- gsub(" ","_", as.character(dem_data$Share_Other_Ethnicity))
dem_data$Share_Other_Religion <- gsub(" ","_", as.character(dem_data$Share_Other_Religion))


dem_data$`Spiritual but unaffiliated` <- gsub("Spiritual but unaffiliated", "Spiritual_but_unaffiliated",as.character(dem_data$`Spiritual but unaffiliated`))
dem_data$PNTA_Religion <- gsub("Prefer not to respond", "Prefer_not_to_respond",as.character(dem_data$PNTA_Religion))



#first eth/relig columns
dem_data <- dem_data %>% mutate(Ethnicity = coalesce(dem_data$Latinx, dem_data$`Black or African American`, dem_data$`Native American/Indigenous`, dem_data$`Pacific Islander`, dem_data$`Middle Eastern`, dem_data$`East Asian`, dem_data$`Southeast Asian`, dem_data$`South Asian`, dem_data$`West Asian`, dem_data$White, dem_data$Other_Ethnicity, dem_data$PNTA_Ethnicity))
dem_data <- dem_data %>% mutate(Religion = coalesce(dem_data$Agnostic, dem_data$Atheist, dem_data$Buddhist, dem_data$Christian, dem_data$Hindu, dem_data$Jewish, dem_data$Muslim, dem_data$Sikh, dem_data$`Spiritual but unaffiliated`, dem_data$Other_Religion, dem_data$PNTA_Religion))

dem_data$Ethnicity <- replace_na(dem_data$Ethnicity, "No Response")
dem_data$Religion <- replace_na(dem_data$Religion, "No Response")



#second sth/relig columns
dem_data <- dem_data %>% mutate(Ethnicity_2 = paste(dem_data$Latinx, dem_data$`Black or African American`, dem_data$`Native American/Indigenous`, dem_data$`Pacific Islander`, dem_data$`Middle Eastern`, dem_data$`East Asian`, dem_data$`Southeast Asian`, dem_data$`South Asian`, dem_data$`West Asian`, dem_data$White, dem_data$Other_Ethnicity, dem_data$PNTA_Ethnicity))
dem_data <- dem_data %>% mutate(Religion_2 = paste(dem_data$Agnostic, dem_data$Atheist, dem_data$Buddhist, dem_data$Christian, dem_data$Hindu, dem_data$Jewish, dem_data$Muslim, dem_data$Sikh, dem_data$`Spiritual but unaffiliated`, dem_data$Other_Religion, dem_data$PNTA_Religion))

dem_data$Ethnicity_2 <- gsub("NA","", as.character(dem_data$Ethnicity_2))
dem_data$Religion_2 <- gsub("NA","", as.character(dem_data$Religion_2))

dem_data$Ethnicity_2 <- str_remove_all(dem_data$Ethnicity_2," ")
dem_data$Religion_2 <- str_remove_all(dem_data$Religion_2," ")

dem_data$Ethnicity_2 <- sub("^$", "No Response", dem_data$Ethnicity_2)
dem_data$Religion_2 <- sub("^$", "No Response", dem_data$Religion_2)


#get rid of bigass ethnicity and religion columns
dem_data <- dem_data %>% select(1, 2, 15:22, 34:38)

#ethnicity
dem_data$Ethnicity <- replace(dem_data$Ethnicity, which(dem_data$Ethnicity_2 != dem_data$Ethnicity), "Mixed")

#religion
dem_data$Religion <- replace(dem_data$Religion, which(dem_data$Religion_2 != dem_data$Religion), "Multiple")

write_xlsx(dem_data_2, "dem_data_2.xlsx")

#Get rid of second columns
drop <- c("Ethnicity_2", "Religion_2")
dem_data = dem_data[,!(names(dem_data) %in% drop)]

#change _ to spaces
dem_data$Ethnicity <- gsub("_"," ", as.character(dem_data$Ethnicity))
dem_data$Religion <- gsub("_"," ", as.character(dem_data$Religion))

#change order
dem_data <- dem_data %>% relocate("Ethnicity", .before = "Share_Other_Ethnicity")
dem_data <- dem_data %>% relocate("Religion", .before = "Share_Other_Religion")

#for the stupid ass . submissions
#dem_data[342, 1] = ".."

#repeats
dem_data_2 <- dem_data[!duplicated(dem_data$GUID), ]


#download dataset
write_xlsx(dem_data_2, "Cleaned Applicant Dataset.xlsx")

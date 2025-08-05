setwd("/Users/polinadorfman/Desktop/Summer research")

library(tidyverse)
library(readr)
library(tidyverse)


#2021
mhlw21 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(21).csv",
                   col_names = FALSE, skip = 6)

col_names21 <- mhlw21 %>% 
  slice(1) %>% 
  unlist(use.names = FALSE)

mhlw21 <- mhlw21 %>%
  slice(41:43) %>%
  setNames(col_names21) %>%
  rename(sex = 2) %>%
  rename_with(tolower) %>%
  mutate(year = 2021) 

mhlw21 <- mhlw21 %>%
  select(-c("causes of death (code) and sex", total, "0year", "1year", "2years", "3years", "4years", 
            "1-4years", "0-4years", "5-9years", "95-99years", "100years and over"),
         -c(starts_with("(re"))) %>%
  relocate(year, .before = 1) %>%
  rename_with(~ str_remove(.x, "years$"))


#2020
mhlw20 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(20).csv",
                   col_names = FALSE, skip = 6)

col_names20 <- mhlw20 %>% 
  slice(1) %>% 
  unlist(use.names = FALSE)

mhlw20 <- mhlw20 %>%
  slice(41:43) %>%
  setNames(col_names20) %>%
  rename(sex = 2) %>%
  rename_with(tolower) %>%
  mutate(year = 2020) 

mhlw20 <- mhlw20 %>%
  select(-c("causes of death (code) and sex", total, "0year", "1year", "2years", "3years", "4years", 
            "1-4years", "0-4years", "5-9years", "95-99years", "100years and over"),
         -c(starts_with("(re"))) %>%
  relocate(year, .before = 1) %>%
  rename_with(~ str_remove(.x, "years$"))


#2019
mhlw19 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(19).csv",
                   col_names = FALSE, skip = 6)

col_names19 <- mhlw19 %>% 
  slice(1) %>% 
  unlist(use.names = FALSE)

mhlw19 <- mhlw19 %>%
  slice(41:43) %>%
  setNames(col_names19) %>%
  rename(sex = 2) %>%
  rename_with(tolower) %>%
  mutate(year = 2019) 

mhlw19 <- mhlw19 %>%
  select(-c("causes of death (code) and sex", total, "0year", "1year", "2years", "3years", "4years", 
            "1-4years", "0-4years", "5-9years", "95-99years", "100years and over"),
         -c(starts_with("(re"))) %>%
  relocate(year, .before = 1) %>%
  rename_with(~ str_remove(.x, "years$"))


#2018
mhlw18 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(18).csv",
                   col_names = FALSE, skip = 6)

mhlw18 <- mhlw18 %>%
  select(X1, X2, X12:X28) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2018)

mhlw18 <- mhlw18 %>%
  select(-X1) %>%
  relocate(year, .before = 1) 


#2017
mhlw17 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(17).csv",
                   col_names = FALSE, skip = 2)

mhlw17 <- mhlw17 %>%
  select(X1, X2, X12:X28) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2017)

mhlw17 <- mhlw17 %>%
  select(-X1) %>%
  relocate(year, .before = 1) 


#2016
mhlw16 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(16).csv",
                   col_names = FALSE, skip = 2)

mhlw16 <- mhlw16 %>%
  select(X1, X2, X12:X28) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2016)

mhlw16 <- mhlw16 %>%
  select(-X1) %>%
  relocate(year, .before = 1) 


#2015
mhlw15 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(15).csv",
                   col_names = FALSE, skip = 2)

mhlw15 <- mhlw15 %>%
  select(X1, X2, X12:X28) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2015)

mhlw15 <- mhlw15 %>%
  select(-X1) %>%
  relocate(year, .before = 1) 


#2014
mhlw14 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(14).csv",
                   col_names = FALSE, skip = 2)

mhlw14 <- mhlw14 %>%
  select(X1, X2, X12:X28) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2014)

mhlw14 <- mhlw14 %>%
  select(-X1) %>%
  relocate(year, .before = 1) 


#2013
mhlw13 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(13).csv",
                   col_names = FALSE, skip = 2)

mhlw13 <- mhlw13 %>%
  select(X1, X2, X12:X28) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2013)

mhlw13 <- mhlw13 %>%
  select(-X1) %>%
  relocate(year, .before = 1) 

#2012
mhlw12 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(12).csv",
                   col_names = FALSE, skip = 2)

mhlw12 <- mhlw12 %>%
  select(X1, X2, X12:X28) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2012)

mhlw12 <- mhlw12 %>%
  select(-X1) %>%
  relocate(year, .before = 1) 


#2011
mhlw11 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(11).csv",
                   col_names = FALSE, skip = 4, col_select = -1)

mhlw11 <- mhlw11 %>%
  select(X2, X3, X13:X29) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2011)

mhlw11 <- mhlw11 %>%
  select(-X2) %>%
  relocate(year, .before = 1) 

#2010
mhlw10 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(10).csv",
                   col_names = FALSE, skip = 4, col_select = -1)

mhlw10 <- mhlw10 %>%
  select(X2, X3, X13:X29) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2010)

mhlw10 <- mhlw10 %>%
  select(-X2) %>%
  relocate(year, .before = 1)


#2009
mhlw09 <- read_csv("/Users/polinadorfman/Desktop/Summer research/Vital Statistics/emc160000(09).csv",
                   col_names = FALSE, skip = 4, col_select = -1)

mhlw09 <- mhlw09 %>%
  select(X2, X3, X13:X29) %>%
  rename(sex = 2,
         "10-14" = 3,
         "15-19" = 4,
         "20-24" = 5,
         "25-29" = 6,
         "30-34" = 7,
         "35-39" = 8,
         "40-44" = 9,
         "45-49" = 10,
         "50-54" = 11,
         "55-59" = 12,
         "60-64" = 13,
         "65-69" = 14,
         "70-74" = 15,
         "75-79" = 16,
         "80-84" = 17,
         "85-89" = 18,
         "90-94" = 19) %>%
  slice(41:43) %>%
  mutate(year = 2009)

mhlw09 <- mhlw09 %>%
  select(-X2) %>%
  relocate(year, .before = 1)


#Combining the MHLW data
mhlw_full <- bind_rows(mhlw09, mhlw10, mhlw11, mhlw12, mhlw13, mhlw14,
                       mhlw15, mhlw16, mhlw17, mhlw18, mhlw19, mhlw20,
                       mhlw21) %>%
  filter(sex != "Total")

#Cleaning the HMD data
hmd <- read_csv("/Users/polinadorfman/Desktop/Summer research/JPN_m_short_idr.csv")

hmd <- hmd %>%
  filter(cause == "S002",
         year >= 2009, 
         sex != 3) %>%
  select(-c(country, cause, list, agf, total, m0, m1, m5, m95),
         -c(ends_with("p")))

hmd <- hmd %>%
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female")) 

%>%
  rename(sex = 2,
         "10-14" = "m10",
         "15-19" = "m15",
         "20-24" = "m20",
         "25-29" = "m25",
         "30-34" = "m30",
         "35-39" = "m35",
         "40-44" = "m40",
         "45-49" = "m45",
         "50-54" = "m50",
         "55-59" = "m55",
         "60-64" = "m60",
         "65-69" = "m65",
         "70-74" = "m70",
         "75-79" = "m75",
         "80-84" = "m80",
         "85-89" = "m85",
         "90-94" = "m90")
  




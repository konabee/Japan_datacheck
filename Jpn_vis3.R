library(tidyverse)
setwd("/Users/polinadorfman/Desktop/Summer research")


#Difference in rates
data_comp <- read.csv("/Users/polinadorfman/Desktop/Summer research/jpn_gov_suiciderate(Sheet1).csv", skip = 1)

data_comp1 <- data_comp %>%
  rename_with(tolower) %>%
  select(agegroup, sex, year, diff) %>%
  mutate(agegroup = factor(agegroup, 
                           levels = unique(agegroup))) %>%
  filter(!agegroup %in% c("total ", "95 to 99", "100+"),
         sex != "both") %>%
  mutate(agegroup = str_replace_all(agegroup, " to ", "-"),
         agegroup = recode(agegroup, "25-29 " = "25-29"))

ggplot(data_comp1, aes(x = year, y = agegroup, fill = diff)) +
  geom_tile() +
  facet_wrap(~ sex, labeller = labeller(sex = c("male" = "Male", "female" = "Female"))) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2000, 2020, by = 2)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  labs(
    x = "Year",
    y = "Age Group",
    fill = "Difference in\ndeath rate\n(per 100,000)",
    title = "Difference Between HMD and Japanese Government Estimates\nof Japan's Suicide Rates (by Sex, Year, and Age Group)",
    subtitle = "The darkest blue corresponds to the greatest difference in rates\nby which the HMD estimates exceed those of the Japanese Government.") +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12))


#Percent difference 
data_perc <- data_comp %>%
  rename_with(tolower) %>%
  select(agegroup, sex, year, rate, diff) %>%
  mutate(agegroup = factor(agegroup, 
                           levels = unique(agegroup))) %>%
  filter(!agegroup %in% c("total ", "95 to 99", "100+"),
         sex != "both") %>%
  mutate(rate = as.numeric(rate))

data_perc1 <- data_perc %>%
  mutate(pct = round((diff / rate) * 100, 2))

data_perc2 <- data_perc1 %>%
  mutate(agegroup = str_replace_all(agegroup, " to ", "-"),
         agegroup = recode(agegroup, "25-29 " = "25-29"))

ggplot(data_perc2, aes(x = year, y = agegroup, fill = pct)) +
  geom_tile() +
  facet_wrap(~ sex, labeller = labeller(sex = c("male" = "Male", "female" = "Female"))) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(
    x = "Year",
    y = "Age Group",
    fill = "Difference (%)",
    title = "Difference in Estimates of Japan's Suicide Rates\nby Sex, Age, and Year",
    subtitle = "Difference between HMD and Japanese MHLW estimates,\nexpressed as a percenatge of the MHLW estimates.") +
  theme_minimal(base_family = "roboto") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))


#MHLW estimates of suicide rates
data_mhlw <- data_comp %>%
  select(agegroup, year, sex, rate) %>%
  filter(!agegroup %in% c("total ", "95 to 99", "100+"),
         sex != "both") %>%
  mutate(sex = fct_relevel(sex, "female", "male"),
         rate = as.numeric(rate),
         agegroup = factor(agegroup, 
                           levels = unique(agegroup)),
         agegroup = str_replace_all(agegroup, " to ", "-"),
         agegroup = recode(agegroup, "25-29 " = "25-29"))

ggplot(data_mhlw, aes(y = agegroup, x = year, fill = rate)) +
  geom_tile() +
  scale_fill_gradient(low="white", high = "#9b2226") +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~sex, labeller = labeller(sex = c("female" = "Female", "male" = "Male"))) +
  labs(title = "MHLW Estimates of Japan's Suicide Rate,\nby Sex, Age, and Year",
       x = "Year",
       y = "Age Group",
       fill = "Death Rate\n(per 100,000)") +
  theme_minimal(base_family = "roboto") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))

#HMD estimates of suicide rates
japan_i <- read.csv("/Users/polinadorfman/Desktop/Summer research/JPN_m_interm_idr.csv")

japan_fil <- japan_i %>%
  filter(cause == "I054",
         year >= 2000 & year <= 2019) %>%
  select(!c(country, list, agf, cause, m0, m1, m5, m85p, m90p, m95p, m95, m100p))

japan_long <- japan_fil %>%
  pivot_longer(cols = c(starts_with("m"), total),
               names_to = "agegroup", 
               values_to = "rate") %>%
  mutate(agegroup = factor(agegroup, 
                           levels = unique(agegroup)))

japan_long1 <- japan_long %>%
  mutate(agegroup = recode(agegroup,
                           "m10" = "10 to 14",
                           "m15" = "15 to 19",
                           "m20" = "20 to 24",
                           "m25" = "25 to 29",
                           "m30" = "30 to 34",
                           "m35" = "35 to 39",
                           "m40" = "40 to 44",
                           "m45" = "45 to 49",
                           "m50" = "50 to 54",
                           "m55" = "55 to 59",
                           "m60" = "60 to 64",
                           "m65" = "65 to 69",
                           "m70" = "70 to 74",
                           "m75" = "75 to 79",
                           "m80" = "80 to 84",
                           "m85" = "85 to 89",
                           "m90" = "90 to 94"))

japan_long2 <- japan_long1 %>%
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    sex == 3 ~ "both"),
    sex = factor(sex, levels = c("both", "male", "female")),
    agegroup = fct_relevel(agegroup, "total")) %>%
  select(agegroup, sex, year, rate) %>%
  arrange(sex, agegroup)

japan_long3 <- japan_long2 %>%
  mutate(rate = round(rate / 10, 1))

jap_heat <- japan_long3 %>%
  filter(agegroup != "total",
         sex != "both") %>%
  mutate(sex = fct_relevel(sex, "female", "male"),
         agegroup = str_replace_all(agegroup, " to ", "-"))

ggplot(jap_heat, aes(y = agegroup, x = year, fill = rate)) +
  geom_tile() +
  scale_fill_gradient(low="white", high = "#9b2226") +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~sex, labeller = labeller(sex = c("female" = "Female", "male" = "Male"))) +
  labs(title = "HMD Estimates of Japan's Suicide Rate,\nby Sex, Age, and Year",
       x = "Year",
       y = "Age Group",
       fill = "Death Rate\n(per 100,000)") +
  theme_minimal(base_family = "roboto") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        plot.title = element_text(size = 16, hjust = 0.5), 
        strip.text = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))

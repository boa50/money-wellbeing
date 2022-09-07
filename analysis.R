library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

df <- read_csv("datasets/income_wellbeing.csv")

df_personcount <- df %>% select(ends_with(".personcount"))
df_wellbeing <- df %>%
  select(household_income, log_household_income, experienced_wellbeing.Mean,
         life_satisfaction.Mean, experienced_wellbeing_zscore.Mean,
         life_satisfaction_zscore.Mean) %>% 
  rename_with(~ gsub(".Mean", "", .))

### Trying to replicate the chart from the paper
x_labels <- df_wellbeing$household_income[1] * 2^c(0:5)
df_wellbeing %>% 
  select(household_income, log_household_income, experienced_wellbeing_zscore,
         life_satisfaction_zscore) %>% 
  pivot_longer(-c(household_income, log_household_income),
               values_to = "value", names_to = "metric") %>%  
  ggplot(aes(x = log_household_income, y = value, color = metric)) +
  geom_line() +
  scale_x_continuous(name = "Income", breaks = log(x_labels), labels = dollar(x_labels)) +
  scale_y_continuous(name = "Score") +
  scale_color_manual(labels = c("Experienced Well-Being", "Life Satisfaction"),
                     values = c("#ef5350", "#42a5f5")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.15))
### Define a fixed width to the chart

### Test convertign the values based on inflation

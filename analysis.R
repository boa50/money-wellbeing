library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

df <- read_csv("datasets/income_wellbeing.csv")

df_personcount <- df %>% select(ends_with(".personcount"))
df_wellbeing <- df %>%
  select(household_income, log_household_income, 
         experienced_wellbeing_zscore.Mean,
         life_satisfaction_zscore.Mean) %>% 
  rename_with(~ gsub(".Mean", "", .)) %>% 
  pivot_longer(-c(household_income, log_household_income),
               values_to = "value", names_to = "metric")

x_labels <- min(df_wellbeing$household_income) * 2^c(0:5)

plot_lines <- function(dataset, x = "log_household_income", 
                       scale_x_param = scale_x_continuous()) {
  ggplot(dataset, aes(x = !!sym(x), y = value, color = metric)) +
    geom_line() +
    scale_x_param +
    scale_y_continuous(name = "Score") +
    scale_color_manual(labels = c("Experienced Well-Being", "Life Satisfaction"),
                       values = c("#ef5350", "#42a5f5")) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.15))
}

### Trying to replicate the chart from the paper
plot_lines(df_wellbeing, x = "log_household_income", 
           scale_x_param = scale_x_continuous(name = "Income", 
                                              breaks = log(x_labels), 
                                              labels = dollar(x_labels)))
### Define a fixed width to the chart when saving the image

### Adjusting the scale
plot_lines(df_wellbeing, x = "household_income", 
           scale_x_param = scale_x_continuous(name = "Income", 
                                              breaks = x_labels[c(1,4:6)], 
                                              labels = dollar(x_labels[c(1,4:6)])))

### Test converting the values based on inflation
### Plot distribution of respondents per money income
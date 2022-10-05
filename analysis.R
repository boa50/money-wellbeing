library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

theme_boa <- function() {
  light_grey <- "#9e9e9e"
  grey <- "#616161"
  
  theme_classic() +
    theme(plot.title = element_text(hjust = -0.14, colour = grey),
          axis.line = element_line(colour = light_grey),
          axis.ticks = element_line(colour = light_grey),
          axis.text = element_text(colour = light_grey),
          axis.title = element_text(colour = light_grey))
}

df <- read_csv("datasets/income_wellbeing.csv")

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
                       values = c("#90caf9", "#b0bec5")) +
    theme_boa() +
    theme(legend.position = "none",
          plot.margin = unit(c(.5,6.5,.5,.5), "lines")) +
    coord_cartesian(clip = "off")
}

### Trying to replicate the chart from the paper
get_last_score <- function(metric_name) {
  df_wellbeing %>% 
    filter(metric == metric_name) %>% 
    filter(row_number() == n()) %>% 
    pull(value)
}

plot_lines(df_wellbeing, x = "log_household_income", 
           scale_x_param = scale_x_continuous(name = "Income", 
                                              breaks = log(x_labels), 
                                              labels = dollar(x_labels))) +
  annotate("text", label = "Experienced Well-Being", hjust = .15,
           size = 3, color = "#90caf9", fontface = "bold",
           x = Inf, y = get_last_score("experienced_wellbeing_zscore")) +
  annotate("text", label = "Life Satisfaction", hjust = .21,
           size = 3, color = "#b0bec5", fontface = "bold",
           x = Inf, y = get_last_score("life_satisfaction_zscore"))

### Define a fixed width to the chart when saving the image

### Adjusting the scale
plot_lines(df_wellbeing, x = "household_income", 
           scale_x_param = scale_x_continuous(name = "Income", 
                                              breaks = x_labels[c(1,4:6)], 
                                              labels = dollar(x_labels[c(1,4:6)]))) +
  annotate("text", label = "Experienced Well-Being", hjust = .15,
           size = 3, color = "#90caf9", fontface = "bold",
           x = Inf, y = get_last_score("experienced_wellbeing_zscore")) +
  annotate("text", label = "Life Satisfaction", hjust = .21,
           size = 3, color = "#b0bec5", fontface = "bold",
           x = Inf, y = get_last_score("life_satisfaction_zscore"))


### Plot distribution of respondents per money income
df_personcount <- df %>% 
  select("household_income", ends_with(".personcount")) %>% 
  mutate(person_count = (select(df, ends_with(".personcount")) %>% do.call(pmax, .)),
         household_income_k = paste(floor(household_income / 1e3), "k")) %>% 
  select(-ends_with(".personcount"))

df_personcount %>% 
  ggplot(aes(x = factor(household_income_k, levels = household_income_k), y = person_count)) + 
  geom_col() + 
  labs(title = "Respondents per household income",
       y = "Respondents", x = "Household Income") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme_boa() +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

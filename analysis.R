library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

theme_boa <- function(title_hjust = -0.14) {
  light_grey <- "#9e9e9e"
  grey <- "#616161"
  
  theme_classic() +
    theme(plot.title = element_text(hjust = title_hjust, colour = grey),
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

wellbeing <- list(
  metric = "experienced_wellbeing_zscore",
  label = "Experienced Well-Being",
  color = "#90caf9",
  label_hjust = .15
)

life_satisfaction <- list(
  metric = "life_satisfaction_zscore",
  label = "Life Satisfaction",
  color = "#b0bec5",
  label_hjust = .21
)

get_last_score <- function(metric_name) {
  df_wellbeing %>% 
    filter(metric == metric_name) %>% 
    filter(row_number() == n()) %>% 
    pull(value)
}

annotate_line <- function(obj) {
  annotate("text", label = obj$label, hjust = obj$label_hjust,
           size = 3, color = obj$color, fontface = "bold",
           x = Inf, y = get_last_score(obj$metric))
}

plot_lines <- function(dataset, x = "log_household_income", 
                       scale_x_param = scale_x_continuous()) {
  ggplot(dataset, aes(x = !!sym(x), y = value, color = metric)) +
    geom_line() +
    scale_x_param +
    labs(title = "Respondents per household income",
         y = "Score", x = "Household Income") +
    scale_color_manual(values = c(wellbeing$color, life_satisfaction$color)) +
    theme_boa(title_hjust = -0.20) +
    theme(legend.position = "none",
          plot.margin = unit(c(.5,6.5,.5,.5), "lines")) +
    coord_cartesian(clip = "off") +
    annotate_line(wellbeing) +
    annotate_line(life_satisfaction)
}

### Trying to replicate the chart from the paper
plot_lines(df_wellbeing, x = "log_household_income", 
           scale_x_param = scale_x_continuous(breaks = log(x_labels), 
                                              labels = dollar(x_labels)))

### Define a fixed width to the chart when saving the image

### Adjusting the scale
plot_lines(df_wellbeing, x = "household_income", 
           scale_x_param = scale_x_continuous(breaks = x_labels[c(1,4:6)], 
                                              labels = dollar(x_labels[c(1,4:6)])))


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
  scale_y_continuous(expand = expansion(mult = c(0, .08))) +
  theme_boa() +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank())
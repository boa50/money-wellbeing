library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

my_colors <- list(
  light_grey = "#9e9e9e",
  grey = "#616161",
  orange = "#fb8c00"
)

theme_boa <- function(title_hjust = -0.14) {
  theme_classic() +
    theme(plot.title = element_text(hjust = title_hjust, colour = my_colors$grey),
          axis.line = element_line(colour = my_colors$light_grey),
          axis.ticks = element_line(colour = my_colors$light_grey),
          axis.text = element_text(colour = my_colors$light_grey),
          axis.title = element_text(colour = my_colors$light_grey))
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
    labs(y = "Score", x = "Household Income") +
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
used_labels <- x_labels[c(1,4:6)]
plot_lines(df_wellbeing, x = "household_income", 
           scale_x_param = scale_x_continuous(breaks = used_labels, 
                                              labels = dollar(used_labels)))


### Plot distribution of respondents per money income
df_personcount <- df %>% 
  select("household_income", ends_with(".personcount")) %>% 
  mutate(person_count = (select(df, ends_with(".personcount")) %>% do.call(pmax, .)),
         household_income_k = paste(floor(household_income / 1e3), "k")) %>% 
  select(-ends_with(".personcount"))

get_person_count <- function(income) {
  df_personcount %>% 
    filter(household_income == income) %>% 
    pull(person_count)
}

get_bar_position <- function(income) {
  which(df_personcount$household_income == income, arr.ind=TRUE)
}

label_bar <- function(income) {
  annotate("text", label = get_person_count(income), size = 3, color = "#FFFFFF",
           x = get_bar_position(income), y = get_person_count(income) - 100)
}

df_personcount %>% 
  mutate(change_color = ifelse(person_count < 1000, TRUE, FALSE)) %>% 
  ggplot(aes(x = factor(household_income_k, levels = household_income_k), 
             y = person_count, fill = change_color)) + 
  geom_col() + 
  labs(title = "Respondents per household income",
       y = "Respondents", x = "Household Income") +
  scale_y_continuous(expand = expansion(mult = c(0, .08))) +
  label_bar(400000) + label_bar(625000) +
  scale_fill_manual(values = c(my_colors$grey, my_colors$orange)) +
  theme_boa() +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

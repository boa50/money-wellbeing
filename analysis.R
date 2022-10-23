library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(janitor)
library(ggtext)

my_colors <- list(
  title = "#616161",
  axis = "#9e9e9e",
  main = "#1976d2",
  no_emphasis = "#757575",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  line_complementary = "#78909c"
)

save_plot <- function(plot_name = "myplot") {
  ggsave(paste(plot_name, ".png", sep = ""), width = 3840, height = 2460, units = "px", dpi = 425)
}

theme_boa <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = my_colors$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = my_colors$axis),
          axis.ticks = element_line(colour = my_colors$axis),
          axis.text = element_text(colour = my_colors$axis),
          axis.title = element_text(colour = my_colors$axis))
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
  color = my_colors$line_main,
  label_hjust = .15
)

life_satisfaction <- list(
  metric = "life_satisfaction_zscore",
  label = "Life Satisfaction",
  color = my_colors$line_complementary,
  label_hjust = .21
)

get_score <- function(metric_name, income) {
  df_wellbeing %>% 
    filter(metric == metric_name & household_income == income) %>% 
    pull(value)
}

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
    theme_boa() +
    theme(legend.position = "none",
          plot.margin = unit(c(.5,7,.5,.5), "lines")) +
    coord_cartesian(clip = "off") +
    annotate_line(wellbeing) +
    annotate_line(life_satisfaction)
}

### Trying to replicate the chart from the paper
plot_lines(df_wellbeing, x = "log_household_income", 
           scale_x_param = scale_x_continuous(breaks = log(x_labels), 
                                              labels = dollar(x_labels)))

save_plot("lines_original")

### Adjusting the scale
used_labels <- x_labels[c(1,4:6)]
best_income <- 137500


lines_adjusted <- plot_lines(df_wellbeing, x = "household_income", 
           scale_x_param = scale_x_continuous(breaks = used_labels, 
                                              labels = dollar(used_labels))) 
lines_adjusted

save_plot("lines_scaled")

lines_adjusted +
  geom_point(aes(x = best_income, y = get_score(life_satisfaction$metric, best_income)),
             size = 3, colour = life_satisfaction$color) +
  geom_point(aes(x = best_income, y = get_score(wellbeing$metric, best_income)),
             size = 3, colour = wellbeing$color)

save_plot("lines_scaled_w_points")

### Plot distribution of respondents per money income
df_personcount <- df %>% 
  select("household_income", ends_with(".personcount")) %>% 
  mutate(person_count = (select(df, ends_with(".personcount")) %>% do.call(pmax, .)),
         household_income_k = paste("$", floor(household_income / 1e3), " k", sep = "")) %>% 
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
  scale_fill_manual(values = c(my_colors$no_emphasis, my_colors$divergent)) +
  theme_boa() +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) 

save_plot("personcount_histogram")

### Comparison between GDP and happiness
df_gdp <- read_csv("datasets/WDIData.csv") %>% 
  clean_names() %>% 
  select(c(country_name, indicator_code, x2018:x2020)) %>%
  # Getting only the "GDP per capita, PPP (constant 2017 international $)" indicator
  filter(indicator_code == "NY.GDP.PCAP.PP.KD") %>% 
  # Filtering of the Country groups
  slice(50:n()) %>% 
  # Getting rows with at least 1 not null value
  filter(rowSums(is.na(.)) < 3) %>% 
  # Getting only the last not NA value
  mutate(gdp = do.call(coalesce, rev(across(x2018:x2020)))) %>% 
  select(c(country_name, gdp))

df_happiness <- read_csv2("datasets/happiness_scores.csv") %>% 
  clean_names() %>% 
  select(c(country, happiness_score)) %>% 
  # Remove some characters indicating that the column value was not the last
  mutate(country = gsub("*", "", .$country, fixed = TRUE))

# Checking names that don't match
# There is no corresponding value on df_gdp for Taiwan
df_happiness %>% 
  merge(df_gdp, by.x = "country", by.y = "country_name", all.x = TRUE) %>% 
  filter(is.na(gdp))

# Replacing names that are different
df_gdp$country_name <- df_gdp$country_name %>% 
  gsub("Congo, Rep.", "Congo", ., fixed = TRUE) %>% 
  gsub("Egypt, Arab Rep.", "Egypt", ., fixed = TRUE) %>% 
  gsub("Gambia, The", "Gambia", ., fixed = TRUE) %>% 
  gsub("Hong Kong SAR, China", "Hong Kong", ., fixed = TRUE) %>% 
  gsub("Iran, Islamic Rep.", "Iran", ., fixed = TRUE) %>% 
  gsub("Kyrgyz Republic", "Kyrgyzstan", ., fixed = TRUE) %>% 
  gsub("Lao PDR", "Laos", ., fixed = TRUE) %>% 
  gsub("West Bank and Gaza", "Palestine", ., fixed = TRUE) %>% 
  gsub("Russian Federation", "Russia", ., fixed = TRUE) %>% 
  gsub("Slovak Republic", "Slovakia", ., fixed = TRUE) %>% 
  gsub("Korea, Rep.", "South Korea", ., fixed = TRUE) %>% 
  gsub("Turkiye", "Turkey", ., fixed = TRUE) %>% 
  gsub("Venezuela, RB", "Venezuela", ., fixed = TRUE) %>% 
  gsub("Yemen, Rep.", "Yemen", ., fixed = TRUE)

df_happiness$country <- df_happiness$country %>% 
  gsub("Czechia", "Czech Republic", ., fixed = TRUE) %>% 
  gsub("Eswatini, Kingdom of", "Eswatini", ., fixed = TRUE) %>% 
  gsub("Hong Kong S.A.R. of China", "Hong Kong", ., fixed = TRUE) %>% 
  gsub("Ivory Coast", "Cote d'Ivoire", ., fixed = TRUE) %>% 
  gsub("North Cyprus", "Cyprus", ., fixed = TRUE) %>% 
  gsub("Palestinian Territories", "Palestine", ., fixed = TRUE)

df_happiness_gdp <- df_happiness %>% 
  merge(df_gdp, by.x = "country", by.y = "country_name")

happiness_median <- median(df_happiness_gdp$happiness_score)
gdp_median <- median(df_happiness_gdp$gdp)
gdp_x_labels <- 0:6 * 20000

plot_scatter <- function(df) {
  ggplot(df, aes(x = gdp, y = happiness_score)) +
  geom_segment(x = 0, xend = Inf, 
               y = happiness_median, yend = happiness_median,
               linetype = "longdash", 
               color = my_colors$axis, 
               size = .25,
               arrow = arrow(length = unit(7,"pt"))) +
    annotate("text", label = "Richer", hjust = 1.25, vjust = -0.5,
             size = 3, color = my_colors$axis,
             x = Inf, y = happiness_median) +
    geom_segment(x = gdp_median, xend = gdp_median, 
                 y = 0, yend = Inf,
                 linetype = "longdash", 
                 color = my_colors$axis, 
                 size = .25,
                 arrow = arrow(length = unit(7,"pt"))) +
    annotate("text", label = "Happier", hjust = 1.25, vjust = -0.5, angle = 90,
             size = 3, color = my_colors$axis,
             x = gdp_median, y = Inf) +
    scale_x_continuous(expand = expansion(mult = c(0, .12)), 
                       breaks = gdp_x_labels,
                       labels = dollar(gdp_x_labels)) +
    labs(y = "Happiness Score", x = "GDP per capita") +
    theme_boa() +
    theme(legend.position = "none")
}

df_happiness_gdp %>% 
  mutate(change_color = ifelse(
    .$gdp <= gdp_median & .$happiness_score >= happiness_median, TRUE, FALSE)) %>% 
  plot_scatter() +
  geom_point(size = 3, alpha = 0.5, aes(color = change_color)) +
  scale_color_manual(values = c(my_colors$no_emphasis, my_colors$main)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 10))

save_plot("happier_and_poor")

happiness_range <- list(low = 5.25, high = 6.75)
happiness_y_ticks <- c(0, 2.5, 5.0, happiness_range$low, happiness_range$high, 7.5, 10.0)
happiness_range_country_percent <- nrow(df_happiness_gdp %>% 
       filter(happiness_score >= happiness_range$low & 
                happiness_score <= happiness_range$high)) / nrow(df_happiness_gdp)

df_happiness_gdp %>% 
  plot_scatter() +
  geom_point(size = 3, alpha = 0.5, color = my_colors$no_emphasis) +
  annotate("rect", fill = my_colors$main, alpha = 0.2,
           xmin = 0, xmax = Inf, 
           ymin = happiness_range$low, ymax = happiness_range$high) +
  annotate("text", label = percent(happiness_range_country_percent),
           size = 15, color = my_colors$main, fontface= "bold",
           x = 80000, y = 4, hjust = 1, vjust = 0) +
  annotate("text", label = "of the countries are represented \n by the blue area",
           size = 4, color = my_colors$no_emphasis, 
           x = 80000, y = 4, hjust = -0.02, vjust = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 10),
                     breaks = happiness_y_ticks) +
  theme(axis.text.y = element_markdown(face = ifelse(happiness_y_ticks %in% 
                                                   c(happiness_range$low, happiness_range$high), "bold", "plain")))

save_plot("happier_indifferent")
library(readr)
library(dplyr)
library(tidyr)
library(janitor)

df <- read_csv("datasets/income_wellbeing.csv")

df_wellbeing <- df %>%
  select(household_income, log_household_income, 
         experienced_wellbeing_zscore.Mean,
         life_satisfaction_zscore.Mean) %>% 
  rename_with(~ gsub(".Mean", "", .)) %>% 
  pivot_longer(-c(household_income, log_household_income),
               values_to = "value", names_to = "metric")

saveRDS(df_wellbeing, "data/df_wellbeing.RDS")

df_personcount <- df %>% 
  select("household_income", ends_with(".personcount")) %>% 
  mutate(person_count = (select(df, ends_with(".personcount")) %>% do.call(pmax, .)),
         household_income_k = paste("$", floor(household_income / 1e3), " k", sep = "")) %>% 
  select(-ends_with(".personcount"))

saveRDS(df_personcount, "data/df_personcount.RDS")

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
  mutate(country = gsub("*", "", .$country, fixed = TRUE),
         # Return the decimal points
         happiness_score = happiness_score / 1000)

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

saveRDS(df_happiness_gdp, "data/df_happiness_gdp.RDS")

rm(df, df_wellbeing, df_personcount, df_happiness_gdp, df_happiness, df_gdp)
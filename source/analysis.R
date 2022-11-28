library(tidyverse)
library(ggpubr)

# The functions might be useful for A4
source("../source/a4-helpers.R")
incarceration <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

## Section 2  ---- 
#----------------------------------------------------------------------------#
#
#----------------------------------------------------------------------------#
jail_population_1970 <- incarceration %>% filter(year == "1970") %>% 
  select(year, total_jail_pop) %>% 
  summarize(n = round(sum(total_jail_pop, na.rm = TRUE))) %>% pull(n)

jail_population_2018 <- incarceration %>% filter(year == "2018") %>% 
  select(year, total_jail_pop) %>% 
  summarize(n = round(sum(total_jail_pop, na.rm = TRUE))) %>% pull(n)

change_1970_to_2018 <- jail_population_2018 - jail_population_1970

state_highest_jail_population <- incarceration %>%
  select(state, total_jail_pop) %>% group_by(state) %>%
  summarize(n = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(n == max(n)) %>% pull(state)

state_lowest_jail_population <- incarceration %>%
  select(state, total_jail_pop) %>% group_by(state) %>%
  summarize(n = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(n == min(n)) %>% pull(state)

white_general_population_2018 <- incarceration %>%
  select(year, white_pop_15to64) %>% filter(year == "2018") %>% 
  summarize(n = round(sum(white_pop_15to64, na.rm = TRUE))) %>% pull(n)

black_general_population_2018 <- incarceration %>%
  select(year, black_pop_15to64) %>% filter(year == "2018") %>% 
  summarize(n = round(sum(black_pop_15to64, na.rm = TRUE))) %>% pull(n)

black_white_ratio_general_pop_2018 <- black_general_population_2018 / white_general_population_2018

white_jail_population_2018 <- incarceration %>%
  select(year, white_jail_pop) %>% filter(year == "2018") %>% 
  summarize(n = round(sum(white_jail_pop, na.rm = TRUE))) %>% pull(n)

black_jail_population_2018 <- incarceration %>%
  select(year, black_jail_pop) %>% filter(year == "2018") %>% 
  summarize(n = round(sum(black_jail_pop, na.rm = TRUE))) %>% pull(n)

black_white_ratio_jail_pop_2018 <- black_jail_population_2018 / white_jail_population_2018

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
get_year_jail_pop <- function() {
  data <- incarceration %>% select(year, total_jail_pop)
  return(data)
}

plot_jail_pop_for_us <- function()  {
  data <- get_year_jail_pop()
  chart <- ggplot(data) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
         x = "Year", y = "Total Jail Population", 
         caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018).
         This chart shows that the incrase of jail population in the United States from 1970 to 2018."
    )
  return(chart)
}

figure_1 <- plot_jail_pop_for_us()
figure_1

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  data <- incarceration %>% filter(state == states) %>% 
    select(year, state, total_jail_pop)
  return(data)
}

plot_jail_pop_by_states <- function(states) {
  data <- get_jail_pop_by_states(states)
  chart <- ggplot(data) +
    geom_line(mapping = aes(x = year, y = total_jail_pop, color = state)) +
    labs(title = "Jail Population in U.S. by State (1970-2018)", 
         x = "Year", y = "Total Jail Population", 
         caption = "Figure 2. Jail Population in U.S. by State (1970-2018).
         This chart shows the change of jail population in Washington, Oregon, California, and New York from 1970 to 2018.")
  return(chart)
}

figure_2 <- plot_jail_pop_by_states(c("WA", "OR", "CA", "NY"))
figure_2

## Section 5  ---- 
#----------------------------------------------------------------------------#
# General Population and Prison Population by Race
#----------------------------------------------------------------------------#
get_general_jail_pop_by_race <- function() {
  data <- incarceration %>%
    filter(year == 2018) %>%
    select(white_pop_15to64, black_pop_15to64, white_jail_pop, black_jail_pop) %>%
    mutate(black_white_ratio_general_pop = black_pop_15to64/white_pop_15to64, black_white_ratio_jail_pop = black_jail_pop/white_jail_pop) %>%
    select(black_white_ratio_general_pop, black_white_ratio_jail_pop)
  return(data)
}

plot_general_jail_pop_by_race <- function() {
  data <- get_general_jail_pop_by_race()
  chart1 <- ggplot(data, aes(x = black_white_ratio_general_pop)) + geom_boxplot() +
    labs(title = "General Population Ratio of Black to White in 2018")
  chart2 <- ggplot(data, aes(x = black_white_ratio_jail_pop)) + geom_boxplot() +
    labs(title = "Jail Population Ratio of Black to White in 2018", caption = "Figure 3. General/Jail Population Ratio of Black to White in 2018.
         This chart shows the boxplots of general and jail population ratio of black people to white people in 2018.")
  chart <- ggarrange(chart1, chart2, ncol=1)
  return(chart)
}

figure_3 <- plot_general_jail_pop_by_race()
figure_3

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Jail Population by State
#----------------------------------------------------------------------------#
state_names <- read.csv("C:/Users/taesr/OneDrive/Desktop/info201/assignments/a4-sukyungtae/source/state_names_and_codes.csv")
state_names <- state_names %>% select(Code, State)
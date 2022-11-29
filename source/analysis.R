library(tidyverse)
source("../source/a4-helpers.R")

in.df <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

## Section 2  ---- 
#----------------------------------------------------------------------------#
#
#----------------------------------------------------------------------------#
jail_population_2018 <- in.df %>% filter(year == "2018") %>% 
  select(year, total_jail_pop) %>% 
  summarize(pop = round(sum(total_jail_pop, na.rm = TRUE))) %>% pull(pop)

jail_population_1970 <- in.df %>% filter(year == "1970") %>% 
  select(year, total_jail_pop) %>% 
  summarize(pop = round(sum(total_jail_pop, na.rm = TRUE))) %>% pull(pop)

change_1970_to_2018 <- jail_population_2018 - jail_population_1970

state_highest_jail_population <- in.df %>%
  select(state, total_jail_pop) %>% group_by(state) %>%
  summarize(pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(pop == max(pop)) %>% pull(state)

state_lowest_jail_population <- in.df %>%
  select(state, total_jail_pop) %>% group_by(state) %>%
  summarize(pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(pop == min(pop)) %>% pull(state)

white_general_population_2018 <- in.df %>%
  select(year, white_pop_15to64) %>% filter(year == "2018") %>% 
  summarize(pop = round(sum(white_pop_15to64, na.rm = TRUE))) %>% pull(pop)

black_general_population_2018 <- in.df %>%
  select(year, black_pop_15to64) %>% filter(year == "2018") %>% 
  summarize(pop = round(sum(black_pop_15to64, na.rm = TRUE))) %>% pull(pop)

black_white_ratio_general_pop_2018 <- black_general_population_2018 / white_general_population_2018

white_jail_population_2018 <- in.df %>%
  select(year, white_jail_pop) %>% filter(year == "2018") %>% 
  summarize(pop = round(sum(white_jail_pop, na.rm = TRUE))) %>% pull(pop)

black_jail_population_2018 <- in.df %>%
  select(year, black_jail_pop) %>% filter(year == "2018") %>% 
  summarize(pop = round(sum(black_jail_pop, na.rm = TRUE))) %>% pull(pop)

black_white_ratio_jail_pop_2018 <- black_jail_population_2018 / white_jail_population_2018

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
get_year_jail_pop <- function() {
  data <- in.df %>% select(total_jail_pop, year)
  return(data)
}

plot_jail_pop_for_us <- function()  {
  data <- get_year_jail_pop()
  chart <- ggplot(data) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
         x = "Year", y = "Total Jail Population", 
         caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018).
         This chart shows the incrase of jail population in the United States from 1970 to 2018."
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
  data <- in.df %>% filter(state == states) %>% 
    select(total_jail_pop, year, state)
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
library(ggpubr)

get_general_jail_pop_by_race <- function() {
  data <- in.df %>%
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
st.df <- read.csv("C:/Users/taesr/OneDrive/Desktop/info201/assignments/a4-sukyungtae/source/state_names_and_codes.csv")
st.df <- st.df %>% select(Code, State)

jail_pop_by_states_2018 <- in.df %>%
  filter(year == 2018) %>% select(state, total_jail_pop) %>%
  rename("Code" = state) %>% left_join(st.df) %>%
  rename("states" = State) %>% mutate(state = tolower(states))
map <- map_data("state") %>% rename(state = region) %>%
  left_join(jail_pop_by_states_2018, by = "state")

get_jail_pop_by_states_map <- function() {
  return(map)
}

plot_jail_pop_by_states_map <- function() { 
  data <- get_jail_pop_by_states_map()
  ggplot(data) + geom_polygon(aes(x = long, y = lat, group = group, fill = total_jail_pop)) +
    scale_fill_continuous(name = "Jail Population", limits = c(0, 5000)) +
    labs(title = "Jail Population by State in U.S. (2018)", caption = "Figure 4. Jail Population by State in U.S. (2018).
         This chart shows that the jail population differs across the country and it is the largest in California.")
}

figure_4 <- plot_jail_pop_by_states_map()
figure_4

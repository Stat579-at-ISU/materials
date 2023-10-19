# homework questions?
# midterm?
# last time: factor variables
library(classdata)
library(tidyverse)

# For one type of crime (subset!) plot boxplots of rates by state,
# reorder boxplots by median crime rates

fbi %>%
  filter(type=="motor_vehicle_theft") %>%
  mutate(
    rate = count/population*100000,
    state_abbr = fct_reorder(state_abbr, rate)
  ) -> fbi_mod

fbi_mod %>%
  ggplot(aes(x = state_abbr, y=rate)) +
  geom_boxplot() +
  geom_point(data = filter(fbi_mod, year==2020),
             colour = "red")

######
fbi %>%
  filter(year >= 2016) %>%
  filter(type=="motor_vehicle_theft") %>%
  group_by(state_abbr) %>%
  mutate(
    rate = count/population*100000,
    rate2020= rate[year==2020],
    trend = ifelse(near(rate2020, max(rate)), "up",
              ifelse(near(rate2020, min(rate)), "down", "stable"))
    ) %>%
  ungroup() %>%
  mutate(
    state_abbr = fct_reorder(state_abbr, rate2020)
  ) %>%
  ggplot(aes(x = state_abbr, y=rate)) +
  geom_boxplot() +
  geom_point(aes(colour = year==2020)) +
  facet_grid(~trend, scales="free", space="free")

####
remotes::install_github("heike/classdata")
library(classdata)
summary(happy)
summary(happy$year)

library(tidyverse)
summary(happy)

library(haven)


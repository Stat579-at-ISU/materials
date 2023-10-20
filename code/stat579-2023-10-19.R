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

fbi %>%
  filter(type=="motor_vehicle_theft") %>%
  mutate(
    rate = count/population*100000,
    state_abbr = fct_reorder(state_abbr, rate)
  ) %>%
  ggplot(aes(x = state_abbr, y=rate)) +
  geom_boxplot() +
  geom_point(data = . %>% filter(year==2020),   # doesn't work: filter(., year==2020) ???
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
  geom_point(data=. %>% filter(year==2020), colour="red") +
  facet_grid(~trend, scales="free_x", space="free")

####
remotes::install_github("heike/classdata")
library(classdata)
summary(happy)
summary(happy$year)
happy$year %>% summary()

library(tidyverse)
summary(happy)

library(haven)

#############
# Load the happy data from the classdata package.
data(happy, package="classdata")

# how many variables, how many observations does the data have?
# What do the variables mean?
dim(happy)

# Plot the variable happy.
happy %>% ggplot(aes(x = happy)) + geom_bar()

happiness <- happy
happiness %>% ggplot(aes(x = happy)) + geom_bar()

# Introduce a new variable nhappy that has
# values 1 for not too happy,
# 2 for pretty happy,
# 3 for very happy and NA for missing values.
# There are multiple ways to get to that. Avoid for loops.

happiness <- happiness %>% mutate(
  nhappy = ifelse(happy == "not too happy", 1,
                  ifelse(happy == "pretty happy", 2,
                         ifelse(happy =="very happy", 3, NA)))
)

happiness <- happiness %>% mutate(
  nhappy2 = case_when(happy == "not too happy" ~ 1,
                      happy == "pretty happy" ~ 2,
                      happy =="very happy" ~ 3,
                      .default = NA))

identical(happiness$nhappy2, happiness$nhappy)

happiness <- happiness %>% mutate(
  nhappy3 = as.numeric(happy)
)

identical(happiness$nhappy2, happiness$nhappy3)


happiness <- happiness %>% mutate(
  nhappy4 = ifelse(happy == "not too happy", -1, NA),
  nhappy4 = ifelse(happy == "pretty happy", 0, nhappy4),
  nhappy4 = ifelse(happy == "very happy", 1, nhappy4)
)
identical(happiness$nhappy2, happiness$nhappy4)


# happiness <- happiness %>% mutate(
#   nhappy5 = switch(happy,
#                    "not very happy" = 1,
#                    "pretty happy" = 2,
#                    "very happy"=3))


# Based on the newly introduced numeric scores,
# what is the average happiness of respondents?

happiness %>%
  summarize(
    avg_happy4 = mean(nhappy4, na.rm=TRUE),
    avg_happy = mean(nhappy, na.rm=TRUE)
  )

happiness %>%
  group_by(year, sex) %>%
  summarize(
    avg_happy4 = mean(nhappy4, na.rm=TRUE),
    avg_happy = mean(nhappy, na.rm=TRUE),
    n = n()
  ) %>%
  filter( n > 10) %>%
  ggplot(aes(x = year, y = avg_happy, colour = sex)) +
  geom_point()



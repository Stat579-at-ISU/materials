# project
# homework

###########
library(classdata)
library(tidyverse)
library(haven)

happiness <- happy

happiness %>% count(happy)
# only one way to create the vector nhappy
happiness <- happiness %>% mutate(
  nhappy = ifelse(happy == "not too happy", 1,
                  ifelse(happy == "pretty happy", 2,
                         ifelse(happy =="very happy", 3, NA)))
)
head(happiness$happy)
# really slick way of creating nhappy
happiness <- happiness %>% mutate(
  nhappy = as.numeric(happy)
)

happiness %>% count(happy)
happiness %>% count(nhappy)


happiness %>%
  summarize(
    n = n(),
    missing = sum(is.na(nhappy)),
    mean_happy = mean(nhappy, na.rm=TRUE)
  )

# how does average happiness change over the course of a life time?

happiness %>%
  group_by(age) %>%
  summarize(
    n = n(),  # group size
    missing = sum(is.na(nhappy)),  # number of missing values in nhappy
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  ggplot(aes(x = age, y = mean_happy, label=n)) + geom_point()

plotly::ggplotly()

# Is this relationship different for men and women?

happiness %>%
  group_by(age, sex) %>%
  summarize(
    n = n(),  # group size
    missing = sum(is.na(nhappy)),  # number of missing values in nhappy
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = age, y = mean_happy, label=n, colour = sex)) +
  geom_point()


plotly::ggplotly()

# How is happiness related to time?
# are people now happier than ten years ago?

happiness %>%
  group_by(year, sex) %>%
  summarize(
    n = n(),  # group size
    missing = sum(is.na(nhappy)),  # number of missing values in nhappy
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = year, y = mean_happy, label=n, colour = sex)) +
  geom_point(size=4)


plotly::ggplotly()

# other variables

happiness %>%
  group_by(degree) %>%
  summarize(
    n = n(),  # group size
    missing = sum(is.na(nhappy)),  # number of missing values in nhappy
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = degree, y = mean_happy, label=n)) +
  geom_point(size=4)

plotly::ggplotly()

happiness %>%
  group_by(finrela) %>%
  summarize(
    n = n(),  # group size
    missing = sum(is.na(nhappy)),  # number of missing values in nhappy
    mean_happy = mean(nhappy, na.rm=TRUE),
    se = sd(nhappy, na.rm=TRUE)/sqrt(n)
  ) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = finrela, y = mean_happy, label=n)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin = mean_happy - qt(0.975, df=n-1)*se,
                    ymax = mean_happy + qt(0.975, df=n-1)*se),
                width = 0.25)

# boxplot need the full data set, lots of individual variability
happiness %>%
  ggplot(aes(x = finrela, y = nhappy)) + geom_boxplot()


happiness %>%
  group_by(marital) %>%
  summarize(
    n = n(),  # group size
    missing = sum(is.na(nhappy)),  # number of missing values in nhappy
    mean_happy = mean(nhappy, na.rm=TRUE),
    se = sd(nhappy, na.rm=TRUE)/sqrt(n)
  ) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = marital, y = mean_happy, label=n)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin = mean_happy - qt(0.975, df=n-1)*se,
                    ymax = mean_happy + qt(0.975, df=n-1)*se),
                width = 0.25)


happiness %>%
  mutate(
    marital = fct_reorder(marital, age, .na_rm = TRUE)
  ) %>%
  group_by(marital) %>%
  summarize(
    n = n(),  # group size
    missing = sum(is.na(nhappy)),  # number of missing values in nhappy
    mean_happy = mean(nhappy, na.rm=TRUE),
    se = sd(nhappy, na.rm=TRUE)/sqrt(n)
  ) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = marital, y = mean_happy, label=n)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin = mean_happy - qt(0.975, df=n-1)*se,
                    ymax = mean_happy + qt(0.975, df=n-1)*se),
                width = 0.25)

####
# mutate and group_by

fbi %>% names()
fbi %>%
  ggplot(aes(x = count/population*100000, y = type)) +
  geom_boxplot()

fbi <- fbi %>% mutate(
  rate = count/population*100000
)


fbi %>%
  group_by(type, state_abbr) %>%
  summarize(
    n = n(),  # group size
    missing = sum(is.na(rate)),  # number of missing values in rate
    mean_rate = mean(rate, na.rm=TRUE),
    min_rate = min(rate, na.rm=TRUE),
    max_rate=max(rate, na.rm=TRUE)
  )

x <- sample(100, 10, replace=F)
rank(x)

fbi <- fbi %>%
  group_by(type) %>%
  mutate(
  n = n(),
  rank = rank(rate)
)

fbi %>% View()

fbi <- fbi %>%
  group_by(type, year) %>%
  mutate(
    n = n(),
    rank = rank(rate)
  )

fbi %>% View()


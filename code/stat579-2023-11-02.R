library(classdata)
library(tidyverse)

happiness <- happy
happiness <- happiness %>% mutate(
  nhappy = ifelse(happy == "not too happy", 1,
                  ifelse(happy == "pretty happy", 2,
                         ifelse(happy =="very happy", 3, NA)))
)

happiness %>% count(nhappy)
happiness %>% count(happy)

# 4802 missing values
# how does average happiness change over the course of a life time?

happiness %>%
  ggplot(aes(x = age, y = nhappy)) + geom_jitter(alpha = 0.1)

happiness %>%
  group_by(age) %>%
  summarise(
    n = n(),
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  ggplot(aes(x = age, y = mean_happy)) + geom_point()

plotly::ggplotly()
# Is this relationship different for men and women? Draw plots.

# sex NA dominates the picture (small group sizes)
happiness %>%
  group_by(age, sex) %>%
  summarise(
    n = n(),
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  ggplot(aes(x = age, y = mean_happy, colour=sex)) + geom_point()

happiness %>%
  group_by(age, sex) %>%
  summarise(
    n = n(),
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n > 5) %>%
  ggplot(aes(x = age, y = mean_happy, colour=sex)) + geom_point()


# are people now happier than ten years ago?
# How is happiness related to time?

happiness %>%
  group_by(year, sex) %>%
  summarise(
    n = n(),
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n > 5) %>%
  ggplot(aes(x = year, y = mean_happy, colour=sex)) + geom_point()


happiness %>%
  group_by(degree, sex) %>%
  summarise(
    n = n(),
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n > 5) %>%
  ggplot(aes(x = degree, y = mean_happy, colour=sex)) +
  geom_jitter(size=3)

#  [1] "year"     "age"      "degree"   "finrela"  "happy"
[6] "health"   "marital"  "sex"      "polviews" "partyid"
[11] "wtssall"  "wtssnr"   "nhappy"



happiness %>%
  group_by(finrela, sex) %>%
  summarise(
    n = n(),
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n > 5) %>%
  ggplot(aes(x = finrela, y = mean_happy, colour=sex)) + geom_point()


happiness %>%
  group_by(health, sex) %>%
  summarise(
    n = n(),
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n > 5) %>%
  ggplot(aes(x = health, y = mean_happy, colour=sex)) + geom_point()

happiness %>%
  mutate(
    marital = factor(marital),
    age = as.numeric(age),
    marital = reorder(marital, age,
                      FUN=mean, na.rm=TRUE)
  ) %>%
  group_by(marital, sex) %>%
  summarise(
    n = n(),
    mean_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  filter(n > 5) %>%
  ggplot(aes(x = marital, y = mean_happy, colour=sex)) + geom_point()


# which states are the best states in each year and type of crime (based on the crime rate)?
# which ten states are the best states in each year and type of crime (based on lowest crime rate)?

fbi %>%
  mutate(
    rate = count/population*100000
  ) %>%
  filter(!is.na(rate)) %>%
  group_by(year, type) %>%
  mutate(
    rank = rank(rate)
  )

# mutate + group_by
# introduce rank variable


fbi %>% group_by(type) %>%
  mutate(
    rate = count/population*100000
  ) %>%
  reframe(
    summary = quantile(rate, na.rm=TRUE),
    probs = seq(0, 1, 0.25)
  )




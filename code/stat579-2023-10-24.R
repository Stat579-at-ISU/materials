#####
# homework questions?
url <- "https://raw.githubusercontent.com/Stat579-at-ISU/stat579-at-isu.github.io/master/homework/data/brfss-iowa-2022.csv"
iowa <- read.csv(url)

# Current smoking status is imputed in the variable X_SMOKER3
# (corresponds to variable _SMOKER3 in the codebook).
# Make X_SMOKER3 a factor.
# Relabel levels 1, 2, 3, 4 to Current Smoker, Current Smoker,
# Former Smoker and Never Smoked (yes, Current Smoker is repeated on purpose) and level 9 to NA.
# Describe the relationship between smoking status and age
# (use X_AGE_G - read up on _AGE_G in the codebook) based on an appropriate visualization.

summary(iowa$X_SMOKER3)

factor(iowa$X_SMOKER3) %>% str()

iowa <- iowa %>%
  mutate(
    X_SMOKER3 = ifelse(X_SMOKER3==9, NA, X_SMOKER3)
  )

iowa <- iowa %>%
  mutate(
    X_SMOKER3 = factor(X_SMOKER3)
  ) #%>% select(X_SMOKER3) %>% str()

# Directly rename levels -
# so many things can go wrong ... better not
levels(iowa$X_SMOKER3) <- c("Current Smoker", "Current Smoker", "Former Smoker", "Never Smoked")

str(iowa$X_SMOKER3)
summary(iowa$X_SMOKER3)

# alternative
# start with fresh copy of iowa data
iowa <- read.csv(url)

iowa %>%
  mutate(
    X_SMOKER3 = forcats::fct_recode(as.character(X_SMOKER3),
                                    "Current Smoker"="1",
                                    "Current Smoker"="2",
                                    "Former Smoker"="3")
  ) %>% select(X_SMOKER3) %>% summary()

iowa <- iowa %>%
  mutate(
    X_SMOKER3 = ifelse(X_SMOKER3==9, NA, X_SMOKER3),
    X_SMOKER3 = forcats::fct_recode(as.character(X_SMOKER3),
                                    "Current Smoker"="1",
                                    "Current Smoker"="2",
                                    "Former Smoker"="3",
                                    "Never Smoked"="4")

  )

str(iowa$X_SMOKER3)
summary(iowa$X_SMOKER3)

iowa %>%
  mutate(
    X_SMOKER3 = forcats::fct_recode(X_SMOKER3,
                                    "Currently smoking"="Current smoker")
  )

iowa_sub <- iowa %>%
  filter(SEXVAR==1)
iowa_sub$X_SMOKER3 %>% summary()
2257/nrow(iowa_sub) # assumption: NA have smoked
(2257+197)/nrow(iowa_sub) # assumption: NA have never smoked

2257/(nrow(iowa_sub)-197) # assumption: exclude NAs <- this is for the homework

iowa_sub <- iowa %>%
  filter(SEXVAR==1, X_AGE_G==1)
iowa_sub$X_SMOKER3 %>% summary()

#####
# midterm samples - data available, solutions updated

####

library(classdata)
library(tidyverse)

fbi %>%
  filter(type == "motor_vehicle_theft") %>%
  mutate(
    state_abbr =
      fct_reorder(state_abbr, count/population, .na_rm = TRUE)
  ) %>%
  ggplot(aes(x = state_abbr,
             y = count/population*100000)) +
  geom_boxplot()

###
fbi %>% select(contains("id"))

iowa %>% select(starts_with("X_")) # all derived variables

iowa %>% select(contains("AGE")) %>% names()
###
iowa %>% ggplot(aes(x = SLEPTIM1)) + geom_bar()
iowa <- iowa %>% mutate(
  SLEPTIM1 = ifelse(SLEPTIM1 %in% c(77,99), NA, SLEPTIM1)
)
iowa %>% ggplot(aes(x = SLEPTIM1)) + geom_bar()

iowa <- iowa %>% mutate(
  X_AGE_G = factor(X_AGE_G),
  X_AGE_G = fct_recode(X_AGE_G, "[18,24]"="1")
)
iowa <- iowa %>% mutate(
  X_SMOKER3 = ifelse(X_SMOKER3 %in% c( "Current Smoker","Former Smoker", "Never Smoked"),
                     X_SMOKER3, NA)
)
iowa %>%
  summarize(
    non_smokers = sum(X_SMOKER3=="Never Smoked", na.rm=TRUE),
    num_responses = sum(!is.na(X_SMOKER3)),
    num_missing = sum(is.na(X_SMOKER3)),
    perc_non_smoker = non_smokers/num_responses*100
  ) # 4 columns, 1 row

iowa %>%
  group_by(SEXVAR) %>%
  summarize(
    non_smokers = sum(X_SMOKER3=="Never Smoked", na.rm=TRUE),
    num_responses = sum(!is.na(X_SMOKER3)),
    num_missing = sum(is.na(X_SMOKER3)),
    perc_non_smoker = non_smokers/num_responses*100
  )

iowa %>%
  group_by(SEXVAR, X_AGE_G) %>%
  summarize(
    non_smokers = sum(X_SMOKER3=="Never Smoked", na.rm=TRUE),
    num_responses = sum(!is.na(X_SMOKER3)),
    num_missing = sum(is.na(X_SMOKER3)),
    perc_non_smoker = non_smokers/num_responses*100
  ) %>%
  ggplot(aes(x = X_AGE_G, y = perc_non_smoker, colour=factor(SEXVAR))) +
  geom_point()



iowa$X_AGE_G %>% str()

iowa <- iowa %>% mutate(
  X_AGE_G = factor(X_AGE_G),
  X_AGE_G = fct_recode(X_AGE_G, "[25,34]"="2", "[35,44]"="3", "[45-54]"="4"),
  X_AGE_G = fct_recode(X_AGE_G, "[55,64]"="5", "65+"="6")
)

iowa$X_AGE_G %>% str()

iowa %>%
  group_by(X_AGE_G) %>%
  summarize(
    n = n(),
  SLEPTIM1_avg = mean(SLEPTIM1, na.rm=TRUE),
  SLEPTIM1_sd = sd(SLEPTIM1, na.rm=TRUE),
)

#################
#
remotes::install_github("heike/classdata")
happiness <- happy


# how many variables, how many observations does the data have?

dim(happiness) # 72390 observations    12 variables

# What do the variables mean?
# read up on ?happy



# Plot the variable happy.

happiness %>%
  ggplot(aes(x = happy)) + geom_bar()

# Introduce a new variable nhappy that has
# values 1 for not too happy, 2 for pretty happy, 3 for very happy and NA for missing values.
# There are multiple ways to get to that. Avoid for loops.


happiness <- happiness %>% mutate(
  nhappy = ifelse(happy=="not too happy", 1,
            ifelse(happy=="pretty happy", 2,
              ifelse(happy=="very happy", 3,  NA)))
  )

happiness$nhappy %>% summary()

happiness %>%
  ggplot(aes(x = nhappy)) + geom_bar()

# Based on the newly introduced numeric scores,
# what is the average happiness of respondents?

happiness %>%
  summarize(
    avg_happy = mean(nhappy, na.rm=TRUE)
  )


happiness %>%
  group_by(sex) %>%
  summarize(
    avg_happy = mean(nhappy, na.rm=TRUE)
  )

happiness %>%
  group_by(age) %>%
  summarize(
    avg_happy = mean(nhappy, na.rm=TRUE)
  ) %>%
  ggplot(aes(x = age, y = avg_happy)) + geom_point()


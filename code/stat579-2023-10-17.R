# homework questions?
# midterm?
# last time: structure in missing values
library(classdata)
library(tidyverse)

missing <- fbi %>% filter(is.na(count))

missing %>% ggplot(aes(x = year, y = state_abbr)) + geom_tile(alpha = 0.5)
missing %>% ggplot(aes(x = year, y = type)) + geom_tile(alpha = 0.5)

str(fbi)


summary(fbi$type)
summary(factor(fbi$type))

summary(fbi$state_id)

fbi <- fbi %>% mutate(
  state_id = factor(state_id)
)
summary(fbi$state_id)
# state_id 11, 43

library(tidyverse)
# bit of a hack, in case you don't have write permissions for
.libPaths()

#install.packages("tidyr", lib=".")
library("tidyr", lib.loc = ".")

summary(fbi$year)
fbi$year <- factor(fbi$year)

fbi <- fbi %>% mutate(
  year = factor(year)
)
summary(fbi$year)

summary(as.numeric(fbi$year)) # NOOOOOOOO!!!

is.factor(fbi$year)

is.factor(fbi$type)
is.character(fbi$type)
fbi <- fbi %>% mutate(
  type = factor(type)
)

is.factor(fbi$type)
as.character(fbi$type) %>% str()

fbi$year %>% str()
as.numeric(fbi$year) %>% str()
as.character(fbi$year) %>% str()

is.factor(fbi$type)
fbi$type %>% str()
as.numeric(fbi$type)

levels(fbi$type)
levels(fbi$year)

fbi %>%
  ggplot(aes(x = type, y = count)) + geom_boxplot()

fbi %>% mutate(
  type = fct_reorder(type, count)
) %>%
  ggplot(aes(x = type, y = count)) + geom_boxplot()

# Before:
levels(fbi$type)
# [1] "aggravated_assault"  "arson"               "burglary"
# [4] "homicide"            "larceny"             "motor_vehicle_theft"
# [7] "rape_legacy"         "rape_revised"        "robbery"

fbi <- fbi %>% mutate(
  type = fct_reorder(type, count)
)
fbi$type %>% str()
levels(fbi$type)

levels(fbi$type)
# [1] "homicide"            "arson"
# [3] "rape_legacy"         "rape_revised"
# [5] "robbery"             "aggravated_assault"
# [7] "motor_vehicle_theft" "burglary"
# [9] "larceny"

fbi %>%
  filter(type == "motor_vehicle_theft") %>%
  ggplot(aes(x = state_abbr, y = count/population*100000)) +
  geom_boxplot()

fbi %>%
 # filter(type == "motor_vehicle_theft") %>%
  mutate(
    state_abbr =
      fct_reorder(state_abbr, count/population, .na_rm = TRUE)
  ) %>%
  ggplot(aes(x = state_abbr,
             y = count/population*100000)) +
  geom_boxplot()

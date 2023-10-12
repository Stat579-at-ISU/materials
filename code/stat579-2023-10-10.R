# end of last time's code file
a <- c(2, 3, 15, 1, 20, 5, 8, 9, 10, 1, 3)

which(a %in% c(1,3))
a

which.min(a)
min(a)
which(a == min(a))

#####
# homework #3

library(dplyr)
library(classdata)
slice_max(fbiwide, burglary)

slice_max(fbiwide, burglary, n=3)

# Use the fbi data object to answer the following question.
# The following question can be answered with a combination of
# filter and slice

# Get a subset of the data that includes number of homicides
# for the last five years. Find the rate of homicides,
# extract all states that have a rate of greater than 90% of
# the rates across all states, and plot.

fbiwide %>% filter(year == 2017, 2018)
# Error in `filter()`:
#   ℹ In argument: `2018`.
# Caused by error:
#   ! `..2` must be a logical vector, not the number 2018.

# second element is interpreted by filter as a second constraint
fbiwide %>% filter(year == 2016, year ==2017) # empty result





fbiwide %>% filter(year == 2016) %>% nrow() # 52
fbiwide %>% filter(year == 2017) %>% nrow() # 52
fbiwide %>% filter(year == c(2016,2017)) %>% nrow()

a <- c(2, 3, 15, 1, 20, 5, 8, 9, 10, 1, 3)
a == 1
a == 3
a == c(1,3) # == is not checking for 'one of'!
a == c(1,3, 1,3,1,3,1,3,1,3,1,3)
# Warning message:
#   In a == c(1, 3) :
#   longer object length is not a multiple of shorter object length
a %in% c(1,3)

fbiwide %>% filter(year %in% c(2016,2017)) %>% nrow()


fbiwide %>% filter(year >= 2016) # works!!!!
fbiwide %>%
  filter(year %in% c(2016, 2017, 2018, 2019, 2020))

fbiwide %>%
  filter(year %in% c(2016, 2017, 2018, 2019, 2020)) %>%
  slice_max(homicide/population, prop=0.1)

library(ggplot2)
fbi %>%
  filter(year %in% c(2016, 2017, 2018, 2019, 2020)) %>%
  filter(type=="homicide") %>%
  slice_max(count/population, prop=0.1) %>%
  ggplot(aes(x = year, y = state, fill=count/population)) +
  geom_tile()

a <- c(2, 3, 15, 1, 20, 5, 8, 9, 10, 1, 3)

a_mod <- ifelse(a==20, 2, a) # ifelse(conditional statement, TRUE value, FALSE value)

url <- "https://raw.githubusercontent.com/Stat579-at-ISU/stat579-at-isu.github.io/master/homework/data/brfss-iowa-2022.csv"

iowa <- read.csv(url)

iowa$HEIGHT3 %>% summary()

iowa <- iowa %>% mutate(
  height3 = ifelse(HEIGHT3==9999, NA, HEIGHT3),
  height3 = ifelse(HEIGHT3==7777, NA, height3)
)

iowa <- iowa %>% mutate(
  height3 = ifelse(HEIGHT3==9999 | HEIGHT3==7777, NA, HEIGHT3)
)

iowa <- iowa %>% mutate(
  height3 = ifelse(HEIGHT3 %in% c(9999, 7777), NA, HEIGHT3)
)

############

summary(fbi)

table(is.na(fbi$count))

any(is.na(fbi$year))
all(!is.na(fbi$year))
any(is.na(fbi$count))

summary(fbi)
# number of missing values in count:
sum(is.na(fbi$count))

# pattern?

fbi %>%
  filter(is.na(count)) %>%
  ggplot(aes(x = state_abbr)) +
  geom_bar()

fbi %>%
#  filter(is.na(count)) %>%
  ggplot(aes(x = state_abbr,
             fill= is.na(count))) +
  geom_bar()

fbi %>%
  #  filter(is.na(count)) %>%
  ggplot(aes(x = year,
             fill= is.na(count))) +
  geom_bar()

names(fbi)


fbi %>%
  #  filter(is.na(count)) %>%
  ggplot(aes(x = type,
             fill= is.na(count))) +
  geom_bar()

fbi %>% filter(is.na(count), type=="arson") # 4 missing values

fbi %>%
  filter(!is.na(count)) %>%
  slice(grep("rape_", type)) %>%
  ggplot(aes(x = type,
             y = year)) +
  geom_point()


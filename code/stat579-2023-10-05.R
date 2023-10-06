
#  Use the fbiwide data object to answer the following question:

#  in how many states were fewer cars stolen than robberies made in 2014? (which states are those?)

fbiwide %>%
  filter(motor_vehicle_theft < robbery, year==2014)

fbiwide %>%
  filter(motor_vehicle_theft < robbery, year==2020)

#####

a <- c(4, 1, 15, 3, 20, 5, 8, 9, 10, 1, 3)

a %in% c(1,3)
which(a %in% c(1,3))

which(a == min(a))

which.min(a)

library(classdata)
library(dplyr)
slice_max(fbiwide, burglary)

fbiwide %>% filter(year >= 2016) %>% slice_max(homicide/population, prop=0.1) # same as last session's your turn

a <- 1:4
a
## [1] 1 2 3 4
a[2:3] <- 0
a
## [1] 1 0 0 4
a <- replace(a, which(a == 0), -1)

## [1]  1 -1 -1  4
a <- ifelse(a==-1, 0, a)


fbiwide %>%
  mutate(
    homicide.rate = homicide/population*100000
  )

fbiwide$homicide.rate


(fbiwide <- fbiwide %>%
  mutate(
    homicide.rate = homicide/population*100000
  ))
fbiwide$homicide.rate

# Your turn

# Use the fbi data from the package classdata

# introduce a variable personal into the dataset that is TRUE
# for personal crimes (list them HH!), and FALSE for property crimes.
# Do not use the variable Violent.crime.
fbi <- fbi %>%
  mutate(
    personal =
      type %in% c("aggravated_assault", "homicide",
                  "rape_legacy", "rape_revised", "robbery")
  )

all(fbi$violent_crime == fbi$personal)

# now introduce a variable class into the dataset that has two levels:
# 'personal' and 'property' classifying the types of crimes reported. Think of ifelse or replace.

fbi %>%
  mutate(
    class = ifelse(personal, "personal", "property")
  ) %>% count(class)


x <- c(7, 4, NA, 10)
length(x)

x[order(x)]

sort(x)
##

# Inspect the fbi object.

# Which variable(s) have missing values? how many?
summary(fbi)

any(is.na(fbi$type))

any(is.na(fbi$count))

sum(is.na(fbi$count))

#  Create a subset of the fbi that contains all missing values.
# Can you identify a pattern in the structure of missing values?

missings <- fbi %>% filter(is.na(count))

nrow(missings)
library(ggplot2)
missings %>%
  ggplot(aes(x = state_abbr)) +
  geom_bar()

fbi %>%
  ggplot(aes(x = state_abbr)) +
  geom_bar()

missings %>%
  ggplot(aes(x = year)) +
  geom_bar()

missings %>%
  ggplot(aes(x = factor(year), y=type)) +
  geom_tile()

missings %>% filter(type=="arson")

#####

# Inspect the fbi object. How many variables are there?
# Which type does each of the variables have?

str(fbi)

# Make a summary of Year

summary(fbi$year)

# Make Year a factor variable:
fbi$year <- factor(fbi$year)
summary(fbi)

# Compare summary of Year to the previous result
summary(fbi$year)

# Are there other variables that should be factors (or vice versa)?
fbi <- fbi %>%
  mutate(
    state_id = factor(state_id),
    state_abbr = factor(state_abbr),
    type = factor(type),
    personal = factor(personal)
  )
summary(fbi)

str(fbi)

all(fbi$personal)

levels(fbi$type)
levels(fbi$year)

rm(fbi) # wipe out all changes to fbi, ue the fbi data object from classdata
twoyear <- dplyr::filter(fbi, year %in% c(1980, 2016))


twoyear %>%
  ggplot(aes(x = year, y=count)) + geom_boxplot(aes())

# following the warning:
twoyear %>%
  ggplot(aes(x = year, y=count)) + geom_boxplot(aes(group = year))

twoyear %>%
  ggplot(aes(x = factor(year), y=count)) + geom_boxplot()

# exploit this:

twoyear %>%
  ggplot(aes(x = 1, y=count)) + geom_boxplot()


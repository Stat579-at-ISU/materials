# even
# Hint: have a look at the help for the operator `%%`

a %% 2 == 0

122022 %/% 10000 # month
122022 %% 10000 # year
# remember this for the homework :)

################
#
library(classdata)

filter(fbi, year == 2014) # results in error:
# Error: object 'year' not found

dplyr::filter(fbi, year == 2014) # gives the right subset

library(tidyverse)
filter(fbi, year == 2014) # now it runs

res <- filter(fbi, type == "larceny", state %in% c("Iowa", "Minnesota"))
res %>% View()
# What's %in%?

a <- c(1, 15, 3, 20, 5, 8, 9, 10, 1, 3)

a %in% c(1,3) ## 'is element of'

# Your turn
# Use the fbi data from the classdata package
?fbi
# Get a subset of all crimes in Iowa,
# Plot incidences/rates for one type of crime over time.

iowa <- fbi %>%
  filter(state=="Iowa")

iowa %>%
  filter(type=='motor_vehicle_theft') %>%
  ggplot(aes(x = year, y = count/population*100000,
             colour = type)) + geom_point()
####

fbi %>%
  filter(state%in%c("Minnesota", "Iowa"),
         type=="aggravated_assault") %>%
  ggplot(aes(x = year, y = count/population*100000,
             colour = state_abbr)) + geom_point()


# Get a subset of all crimes in 2009.
# Plot the number or rate for one type of crime by state.

fbi %>%
  filter(year == 2009,
         type=="robbery") %>%
  ggplot(aes(x = state_abbr, y = count/population*100000,
             colour = type)) +
  geom_point()

# Get a subset of the data that includes number of homicides
# for the last five years.
last_five <-
  fbi %>% filter(year %in% 2016:2020, type == "homicide")

# Find the rate of homicides,
last_five$rate <- last_five$count / last_five$population * 100000

# extract all states that have a
# rate of greater than 90% of the rates across all states,
# and plot (Hint: ?quantile).

# a) 90% of the rates:
hr90 <- quantile(last_five$rate, probs = 0.9)

# plot:

last_five %>%
  filter(rate > hr90) %>%
  ggplot(aes(x = state_abbr, y=rate, colour=factor(year))) +
  geom_jitter(size=3) +
  scale_color_brewer(palette="Set2")

# more color schemes at https://colorbrewer2.org



nrow(last_five)
ncol(last_five)

a <- c(2, 3, 15, 1, 20, 5, 8, 9, 10, 1, 3)

which(a %in% c(1,3))
a

which.min(a)
min(a)
which(a == min(a))


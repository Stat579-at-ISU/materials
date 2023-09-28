library(tidyverse)
remotes::install_github("heike/classdata")
# remotes::install_github("heike/classdata")
library(classdata)

dplyr::near(1/10+2/10- 3/10, 0)

# have a look at https://0.30000000000000004.com/ for an overview of how different
# languages handle the machine precision.

# take away: don't compare numeric values for equality using ==, >= or <=

#########
# Your turn

a <- c(1, 15, 3, 20, 5, 8, 9, 10, 1, 3)

# which values in a are even?

a %% 3

which(a %% 2 == 0) # returns the indices of all even numbers in a

a %% 2 == 0

#######

?filter

filter(fbi, state_abbr=="IA")
names(fbi)
dplyr::filter(fbi, state_abbr=="IA")


############
# Your turn

# Get a subset of all crimes in Iowa,
# Plot incidences/rates for one type of crime over time.

fbi %>%
  filter(`state_abbr` %in% c("IA", "MN"),
         type=='aggravated_assault') %>% #nrow() #dim()
  ggplot(aes(x = year, y = count/population*100000, colour = state_abbr)) +
  geom_point() +
  scale_colour_brewer(name="State", palette = "Set1") +
  theme_bw() +
#  theme(legend.position = "none") +
  ylab("Rate (1 in 100,000)") +
  ggtitle("Rate of aggravated assaults in Iowa from 1980 to 2020") +
  ylim(c(0,NA))


# Get a subset of all crimes in 2009.
# Plot the number or rate for one type of crime by state.

fbi %>%
  filter(
    year == "2009",
    type=='aggravated_assault') %>% #nrow() #dim()
  ggplot(aes(x = state_abbr, y = count/population*100000)) +
  geom_point() +
  scale_colour_brewer(name="State", palette = "Set1") +
  theme_bw() +
  #  theme(legend.position = "none") +
  ylab("Rate (1 in 100,000)") +
#  ggtitle("Rate of aggravated assaults in Iowa from 1980 to 2020") +
  ylim(c(0,NA))


# Get a subset of the data that includes number of homicides for the last five years.
# Find the rate of homicides, extract all states that have a rate of greater than 90% of the rates across all states,
# and plot (Hint: ?quantile).

homicides16 <- filter(fbi, year >= 2016, type=="homicide")
homicides16 <- homicides16 %>%
  mutate(
    rate = count/population*100000
  )
threshold <- quantile(homicides16$rate, probs = .9)

homicides16 %>%
  filter(rate > threshold) %>%
  ggplot(aes(x = state_abbr, y = rate, colour = as.factor(year))) +
  geom_point(size=3) +
  theme_bw() +
  scale_color_brewer(palette = "Set1")

####
# Your turn

# Use the fbi data object to answer the following questions:

#  how many reports of Burglaries are there for California?
fbi %>%
  filter(state_abbr=="CA", type=="burglary") %>% nrow()
fbi %>%
  filter(state_abbr=="CA", type=="burglary", !is.na(count)) %>% nrow()

#  for any of the violent crimes, which state had the highest rate (and for which crime) in 2014? in 2020?

fbi %>%
  filter(violent_crime, year %in% c(2014, 2020)) %>%
  mutate(
    rate = count/population*100000
  ) %>% slice_max(rate, n = 5)

# similar to which.max


#  Use the fbiwide data object to answer the following question:

#  in how many states were fewer cars stolen than robberies made in 2014? (which states are those?)

fbiwide %>%
  filter(motor_vehicle_theft < robbery, year==2014)

fbiwide %>%
  filter(motor_vehicle_theft < robbery, year==2020)

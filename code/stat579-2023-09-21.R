library(tidyverse)
remotes::install_github("heike/classdata")
# remotes::install_github("heike/classdata")
library(classdata)

fbiwide |>
  ggplot(aes(x = state_abbr)) +
  geom_bar()

fbiwide |>
  ggplot(aes(x = population)) +
  geom_histogram()


#############
# Plot the number of car thefts by year for each state (facet by state).

fbiwide %>%
  ggplot(aes(x = year, y = motor_vehicle_theft)) +
  geom_point() +
  facet_wrap(~state)


# The numbers are dominated by the number of thefts in California, New York, and Texas. Use a log-scale for the y-axis. Does that help?

fbiwide %>%
  ggplot(aes(x = year, y = log(motor_vehicle_theft, base=10))) +
  geom_point() +
  facet_wrap(~state)


fbiwide %>%
  ggplot(aes(x = year, y = motor_vehicle_theft)) +
  geom_point() +
  facet_wrap(~state) +
  scale_y_continuous(trans="log10")

fbiwide %>%
  ggplot(aes(x = year, y = motor_vehicle_theft)) +
  geom_point() +
  facet_wrap(~state) +
  scale_y_log10()


# Another approach to fix the domination by CA, TX and NY:
# Read up on the parameters in facet_wrap to find a way to give each panel its own scale. Comment on the difference in the results.

?facet_wrap

# parameters: scales =

fbiwide %>%
  ggplot(aes(x = year, y = motor_vehicle_theft)) +
  geom_point() +
  facet_wrap(~state, scales="free_y")


fbiwide %>%
  ggplot(aes(x = year,
             y = motor_vehicle_theft/population*90000)) +
  geom_point() +
  facet_wrap(~state)

###########
# Your turn

# Using ggplot2, draw side-by-side boxplots of the number of robberies by
# state. Use a log transformation on y and compare results.

fbiwide %>%
  ggplot(aes(x = state_abbr, y = robbery)) +
  geom_boxplot()

fbiwide %>%
  ggplot(aes(x = state_abbr, y = robbery)) +
  geom_boxplot() +
  scale_y_log10()


# Compare rates of robberies by state, i.e. adjust robberies by the state population. Then plot side-by-side boxplots.

fbiwide %>%
  ggplot(aes(x = state_abbr, y = robbery/population*100000)) +
  geom_boxplot()

fbiwide %>%
  ggplot(aes(x = factor(year), y = robbery/population*100000)) +
  geom_boxplot()

fbiwide %>%
  ggplot(aes(x = robbery/population*100000)) +
  geom_histogram()

fbiwide %>%
  ggplot(aes(x = robbery/population*100000)) +
  geom_histogram(binwidth=100)

fbiwide %>%
  ggplot(aes(x = robbery/population*100000)) +
  geom_histogram(binwidth=5)

# change the binwidth! inspect large scale shapes and small scale variability (like multiple modes)


#####

F
FALSE

T
TRUE

x <- c(T, T, F, F, T, F)
as.logical(2*x)

x = 2 ### assignment

x == 3

.1 + .05 == .15

1 + 2 == 3
(1 + 2)/10 == 3/10
1/10 + 2/10 == 3/10

1/10 + 2/10 <= 3/10


(1/10 + 2/10) - 3/10

dplyr::near(1/10+2/10- 3/10, 0)



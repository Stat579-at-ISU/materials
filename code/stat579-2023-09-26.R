library(classdata)
library(tidyverse)


########

ggplot(data = fbiwide,
       aes(x = state_abbr,
           y = motor_vehicle_theft/population
           )) +
  geom_boxplot() +
  scale_y_log10()

# might need a different variance stabilizing transformation, but log is better than raw rates.


ggplot(data = fbiwide,
       aes(x = population,
       )) +
  geom_histogram(binwidth=10^6)

ggplot(data = fbiwide,
       aes(x = population,
       )) +
  geom_histogram(binwidth=10^6)


ggplot(data = fbiwide,
       aes(x = population,
       )) +
  geom_histogram(binwidth=5*10^5)

ggplot(data = fbiwide,
       aes(x = population,
       )) +
  geom_histogram(binwidth=10^5)


ggplot(data = fbiwide,
       aes(x = state,
       )) +
  geom_histogram(binwidth=10^5)

ggplot(data = fbiwide,
       aes(x = population,
       )) +
  geom_density(fill="grey50")


ggplot(data = fbi,
       aes(x = type,
         y = population,
       )) +
  geom_violin(fill="grey50") +
  geom_boxplot(width=.25)

ggplot(fbiwide) +
  geom_point(aes(x = motor_vehicle_theft, y = robbery, size=1/population))


ggplot(fbiwide,aes(x = motor_vehicle_theft, y = robbery)) +
  geom_point(alpha = 0.1, size=4, stroke=NA) +
  theme_bw()

##################

fbiwide %>%
  ggplot(aes(x = motor_vehicle_theft, y = robbery)) +
    geom_point(alpha = 0.1, size=4, stroke=NA) +
    theme_bw()

fbiwide %>% ggplot(aes(x = motor_vehicle_theft, y = state_abbr))

fbiwide |> ggplot(aes(x = motor_vehicle_theft, y=aggravated_assault))

###############
# logical vectors
x <- c(2,3,4,5,1,4,7)

x <- 2

x %in% c(1, 4, 3, 7)  # 'is an element of'

FALSE
F

FALSE > FALSE
TRUE
T

res <- x %in% c(1, 4, 3, 7)

2*res

sum(res)

sum(res==FALSE)

res
res==FALSE

x == 2
x != 2


fbi %>% View()
filter(fbi, state_abbr=="IA") %>% head()


a <- c(3,1,5,7, 10, 2)
a == 3
a[a == 3]
a[a >= 3]

a %in% 4:8

(a >= 4) & (a <= 8)
a

(a >= 4) | (a <= 8)


# Your turn
# Define vector a to be
a <- c(1, 15, 3, 20, 5, 8, 9, 10, 1, 3)

# Find the expression for the logical vector that is TRUE where a is:

#  less than 20
a < 20

# squared value is at least 100 or less than 10

(a^2 >= 100) | (a^2 < 10)


# equals 1 or 3

a %in% c(1,3)
(a == 1) | (a == 3)

# even
# Hint: have a look at the help for the operator `%%`

a %% 2 == 0

122022 %/% 10000 # month
122022 %% 10000 # year

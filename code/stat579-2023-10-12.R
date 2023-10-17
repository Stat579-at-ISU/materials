library(classdata)
library(dplyr)
library(ggplot2)

summary(fbi$year)
summary(factor(fbi$year))

twoyear <- dplyr::filter(fbi, year %in% c(1980, 2016))


twoyear %>%
  ggplot(aes(x = year, y=count)) + geom_boxplot(aes())

twoyear %>%
  ggplot(aes(x = factor(year), y=count)) + geom_boxplot(aes())

twoyear %>%
  ggplot(aes(x = year, y=type, colour=year)) + geom_jitter()

twoyear %>%
  ggplot(aes(x = year, y=type, colour=factor(year))) + geom_jitter()

# casting to factors:
twoyear <- twoyear %>%
  mutate(
    year = factor(year),
    type = factor(type)
  )
summary(twoyear)

twoyear %>%
  ggplot(aes(x = year, y=type, colour=factor(year))) + geom_jitter()


twoyear %>%
  ggplot(aes(x = as.numeric(year), y=type, colour=factor(year))) + geom_jitter()

twoyear$type %>%str()

# use number
as.numeric(twoyear$type) %>% str()
# use label
as.character(twoyear$type) %>% str()

# use number
as.numeric(twoyear$year) %>% str()
# use label
as.character(twoyear$year) %>% as.numeric() %>% str()

levels(twoyear$type)

levels(twoyear$type)[4] <- "murder"

?reorder

fbi %>%
  ggplot(aes(x = type, y= log(count))) + geom_boxplot()

fbi %>%
  ggplot(aes(x = reorder(type, count, FUN=median, na.rm=TRUE),
             y= log(count))) +
  geom_boxplot()

levels(fbi$type)
is.null(levels(fbi$type))
is.factor(fbi$type)


fbi <- fbi %>% mutate(
  type = factor(type)
)

levels(fbi$type)


fbi %>%
  ggplot(aes(x = reorder(type, count, FUN=median, na.rm=TRUE),
             y= log(count))) +
  geom_boxplot()

levels(fbi$type)

fbi <- fbi %>% mutate(
  type= reorder(type, count, FUN=median, na.rm=TRUE)
)
levels(fbi$type)

library(forcats)

fbi <- fbi %>% mutate(
  type= fct_reorder(type, count, .na_rm=TRUE)
)

fbi %>%
  ggplot(aes(x = type,
             y= log(count))) +
  geom_boxplot()


# fbi <- fbi %>% mutate(
#   type= factor(type, levels = c("aggravated_assault", "arson", "burglary",
#                                 "homicide", " "....))
# )

fbi$type %>% str()

?fct_infreq

fbi %>%
  ggplot(aes(x = type, weight=count)) +
  geom_bar()


# reorder(x, X, FUN=mean)
# 1. split X by the categories in x,
# 2. apply FUN to each set of X values separately
# 3. reorder the categories in x according to the values in (2)

x <- rep(c("a", "b", "c"), times=c(4,2,6))
plot(table(x))

# reorder(x, x, FUN=length) #
reorder(x, x, FUN=length)

fct_infreq(x)

############
# Introduce a rate of the number of reported offenses by population into the
# fbi data. You could use the Ames standard to make values comparable to a
# city of the size of Ames and Story county (population ~100,000).
fbi <- fbi %>% mutate(
  rate = count/population*100000
)

# Plot boxplots of crime rates by different types of crime.
# How can you make axis text legible?

fbi %>%
  mutate(
    rate = count/population*100000,
    type = fct_reorder(type, rate)
  ) %>%
  ggplot(aes(x = type, y=rate)) + geom_boxplot() +
  scale_y_log10() +
  coord_flip()

fbi %>%
  mutate(
    rate = count/population*100000,
    type = fct_reorder(type, rate)
  ) %>%
  ggplot(aes(x = type, y=rate)) + geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))
# ggplot cook book by Winston Chang: https://r-graphics.org/


# Reorder the boxplots of crime rates, such that the boxplots are
# ordered by their medians.




# For one type of crime (subset!) plot boxplots of rates by state,
# reorder boxplots by median crime rates

fbi %>%
  filter(type=="motor_vehicle_theft") %>%
  mutate(
    rate = count/population*100000,
    state_abbr = fct_reorder(state_abbr, rate)
  ) -> fbi_mod

fbi_mod %>%
  ggplot(aes(x = state_abbr, y=rate)) +
  geom_boxplot() +
  geom_point(data = filter(fbi_mod, year==2020),
             colour = "red")

# fbi %>%
#   filter(year >= 2016) %>%
#   filter(type=="motor_vehicle_theft") %>%
#   group_by(state_abbr) %>%
#   mutate(
#     rate2020= rate[year==2020],
#     trend = ifelse(near(rate2020, max(rate)), "up",
#               ifelse(near(rate2020, min(rate)), "down", "stable"))
#     ) %>%
#   ungroup() %>%
#   mutate(
#     state_abbr = fct_reorder(state_abbr, rate2020)
#   ) %>%
#   ggplot(aes(x = state_abbr, y=rate, color=year)) +
#   geom_point() +
#   facet_grid(~trend, scales="free", space="free")

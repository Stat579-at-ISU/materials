# presentation - sign up!
# participation during presentations
# midterm - extra points
# HH: make a template report!
library(tidyverse)
data(french_fries, package="reshape2")

ff_long <- french_fries %>%
  pivot_longer(cols = potato:painty,
               names_to = "scale", values_to = "score")

ff_rep <- ff_long %>%
  pivot_wider(names_from=rep, values_from = score,
              names_prefix = "Rep ")
head(ff_rep)

ff_rep %>%
  ggplot(aes(x = `Rep 1`, y = `Rep 2`)) + geom_point() +
  facet_wrap(~scale)

ff_rep %>% group_by(scale) %>%
  summarise(
 rep_to_rep_corr =  cor(`Rep 1`, `Rep 2`, use="pairwise")
)

#
# Use pivot_longer from the tidyr package to combine the variables for the
# different types of crimes into one variable.
# Call the key-value pair "Type" and "Incidences". Compute a crime rate
library(classdata)
fbi_long <- fbiwide %>%
  select(-property_crime, -violent_crime) %>%
  pivot_longer(homicide:arson, names_to = "Type", values_to = "Incidences") %>%
  mutate(
    rate = Incidences/population*100000
  )

fbi_long %>%
  filter(state_abbr %in% c("IA", "MN")) %>%
  pivot_wider(values_from="rate", names_from="state_abbr")

two <- fbi_long %>%
  filter(state_abbr %in% c("IA", "MN"))

two %>%
  pivot_wider(values_from=rate, names_from=state_abbr) %>% dim()
# didn't collapse 'keys'

two_wide <- two %>% select(-state, -state_id, -population, -Incidences) %>%
  pivot_wider(values_from=rate, names_from=state_abbr)


df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))

df %>% separate_wider_delim(x, delim=".", names=c("var1", "var2"),
                            cols_remove = FALSE)

url <- "https://github.com/Stat579-at-ISU/materials/blob/master/03_tidyverse/data/Iowa_Liquor_Sales.csv.zip?raw=TRUE"
download.file(url, "iowa.zip", mode="wb")
iowa <- readr::read_csv("iowa.zip")

# Number of variables? number of observations?
dim(iowa)
# How many different stores in Ames order liquor?
iowa %>% count(City)
iowa %>% count(`Store Name`, `Store Number`) %>% dim()
iowa %>% count(`Store Number`) %>% dim()

# What is the time frame of the data?
# hm.

# we could use separate_delim_wider ... but
library(lubridate)
iowa <- iowa %>% mutate(
  proper_date = mdy(Date)
)

iowa %>% select(Date, proper_date) %>% summary()

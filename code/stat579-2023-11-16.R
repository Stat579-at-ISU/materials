# homework? - extension needed?
# project - does everyone have a team?
# presentation - sign up!
library(tidyverse)
data(french_fries, package="reshape2")

ff_long <- french_fries %>%
  pivot_longer(cols = potato:painty,
               names_to = "scale", values_to = "score")

ff_rep <- ff_long %>%
  pivot_wider(names_from=rep, values_from = score,
              names_prefix = "Rep ")

ff_rep %>%
  ggplot(aes(x = `Rep 1`, y = `Rep 2`)) + geom_point() +
  facet_wrap(~scale)

# in write-ups: in the `dataset` data we have `{r} nrow(dataset)` observations and ...
# `{r} mean(dataset$column1)` is the average of column1

############
library(classdata)
fbi_long <- fbiwide %>%
  pivot_longer(cols=c(homicide:aggravated_assault, burglary:arson),
                         names_to="type", values_to="incidences")

two_states <- fbi_long %>% filter(
  state_abbr %in% c("IA", "MN")
)

two_states_wide <- two_states %>%
  pivot_wider(names_from = "state_abbr", values_from="incidences")
complete.cases(two_states_wide[,c("IA", "MN")]) %>% summary()

two_states_wide %>% ggplot(aes(x = IA, y = MN)) + geom_point(aes(colour=year))

two_states_wide_better <- two_states %>%
  select(-state, -state_id, -population, -violent_crime, -property_crime) %>%
  pivot_wider(names_from = "state_abbr", values_from="incidences")


two_states_wide_better %>% ggplot(aes(x = IA, y = MN)) + geom_point(aes(colour=year)) +
  facet_wrap(~type, scales="free")

#####
url <- "https://github.com/Stat579-at-ISU/materials/blob/master/03_tidyverse/data/Iowa_Liquor_Sales.csv.zip?raw=TRUE"
download.file(url, "iowa.zip", mode="wb")
iowa <- readr::read_csv("iowa.zip")

iowa %>% count(City)
length(unique(iowa$`Store Name`))
iowa %>% count(`Store Name`)

iowa %>% count(`Store Name`, `Store Number`)
length(unique(iowa$`Store Number`))

iowa$Date %>% head()
iowa$Date %>% str()
iowa$Date %>% summary()

# for dates and times use the  lubridate package
iowa <- iowa %>% mutate(
  proper_date = mdy(Date)
)

iowa %>% select(proper_date) %>% summary()
ymd("2023/12/25") -today()
wday(ymd("2023/12/25"), label = TRUE)

# geographic information:
iowa$`Store Location` %>% head()


# pixel maps https://github.com/dkahle/ggmap
# leaflet package https://rstudio.github.io/leaflet/


iowa$`Store Location` %>% head()
parse_number(iowa$`Store Location`) %>% head()

iowa <- iowa %>% separate_wider_delim(
  cols=`Store Location`, cols_remove = FALSE,
  delim=" ", names=c("garbage","Longitude", "Latitude")) %>%
  select(-garbage)
iowa <- iowa %>% mutate(
  Longitude = parse_number(Longitude),
  Latitude = parse_number(Latitude)
)

iowa %>%
  ggplot(aes(x = Longitude, y = Latitude)) + geom_point()

iowa <- iowa %>%
  separate_wider_regex(
    cols=`Store Location`, cols_remove = FALSE,
    patterns = c("POINT \\(", Longitude2=".*", " ", Latitude2=".*", "\\)")
  )


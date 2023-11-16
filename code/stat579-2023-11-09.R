# zipping files
path <- "~/Documents/packages/classdata/data-raw/Iowa Liquor Data/"

liquor <- readr::read_csv(
  file.path(path, "2020_Iowa_Liquor_Sales_20231109.csv"))
liquor2 <- readr::read_csv(
  file.path(path, "2020_Iowa_Liquor_Sales_20231109.csv.zip"))

identical(liquor, liquor2) # assume that this is true
##########
library(tidyverse)
data(french_fries, package="reshape2")

french_fries %>%
  ggplot(aes(x = buttery, y = rancid)) + geom_point() +
  xlim(c(0,15))

french_fries %>%
  summarize(
    buttery_max = max(buttery, na.rm = TRUE),
    buttery_min = min(buttery, na.rm=TRUE),
    buttery_missing = sum(is.na(buttery)),
    potato_max = max(potato, na.rm = TRUE),
    potato_min = min(potato, na.rm=TRUE),
    potato_missing = sum(is.na(potato))
  )

?pivot_longer

ff_long <- french_fries %>%
  pivot_longer(cols = 5:9, names_to = "scale", values_to = "score")

dim(french_fries)
dim(ff_long)

ff_long %>%
  group_by(scale) %>%
  summarize(
    score_max = max(score, na.rm = TRUE),
    score_min = min(score, na.rm=TRUE),
    score_missing = sum(is.na(score))
  )


ff_long %>% ggplot(aes(x = scale, y = score)) + geom_boxplot()

lm(score~scale*as.numeric(time), data=ff_long) %>% summary()

ff_long %>% ggplot(aes(x = as.numeric(time),
                       y = score, colour=scale)) +
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~treatment)

library(classdata)
head(fbiwide)

fbiwide %>% pivot_longer(cols=c(homicide:aggravated_assault, burglary:arson),
                         names_to="type", values_to="count")
# 19,466 rows

fbiwide %>% pivot_longer(cols=c(homicide:aggravated_assault, burglary:arson),
                         names_to="type", values_to="count") %>% filter(!is.na(count)) %>% dim() # 17516

dim(fbi) # 19476 rows
fbi %>% filter(!is.na(count)) %>% dim()

ff_long %>%
  pivot_wider(names_from=rep, values_from = score,
              names_prefix = "Rep ") %>%
  ggplot(aes(x = `Rep 1`, y = `Rep 2`)) + geom_point() +
  facet_wrap(~scale)

ff_long %>%
  group_by(time, scale, subject, treatment) %>%
  summarize(n = n(),
            score = mean(score, na.rm=TRUE)) %>%
  pivot_wider(names_from=time, values_from = score,
              names_prefix = "Week ") %>%
  ggplot(aes(x = `Week 1`, y = `Week 10`)) + geom_point() +
  facet_wrap(~scale) +
  geom_abline()

cor(french_fries[, 5:9], use = "pairwise")




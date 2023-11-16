# project!
# homework?

library(classdata)
library(tidyverse)
library(haven)

fbi <- fbi %>%
  mutate(rate = count/population*100000)

fbi %>%
  arrange(rate) %>%
  group_by(type, year) %>%
  mutate(
    n = n(),
    rank = rank(rate)
  ) %>%
  arrange(fbi$rate) # <<- don't use the $ in the tidyverse

fbi %>% View()

# group structure is 'sticky'
# get rid of it with ungroup()

fbi %>% ungroup() %>% count(type)

attr(fbi, "groups")

######
# mutate + group_by

# how many observations does each chick have?

ChickWeight %>%
  group_by(Chick) %>%
  mutate(n = n()) %>% View()

# and use the helper variable for filtering out incomplete data
ChickWeight %>%
  group_by(Chick) %>%
  mutate(n = n()) %>%
  filter(n == 12) -> complete


df <- attr(fbi, "groups")

###########
data(french_fries, package="reshape2")

dim(french_fries)

ff_long <- french_fries %>% pivot_longer(cols=5:9)
ff_long <- french_fries %>% pivot_longer(cols=potato:painty)
ff_long <- french_fries %>% pivot_longer(cols=-(1:4)) %>% head()
names(ff_long)
names(ff_long)[5:6] # maybe rename?

ff_long <- french_fries %>% pivot_longer(cols=5:9,
                                         names_to = "scale", values_to = "score")
ff_long %>% head()
lm(score ~ scale, data = ff_long) %>% summary()

lm(score ~ time, data = ff_long) %>% summary()


ff_long %>%
  ggplot(aes(x = as.numeric(time), y = score, colour = scale)) +
  geom_point() +
  geom_smooth(method="lm")

?pivot_wider

head(ff_long)

by_reps <- ff_long %>% pivot_wider(names_from=rep, values_from=score)

ff_long %>% pivot_wider(names_from=rep, values_from=score, names_prefix="Rep") %>%
  ggplot(aes(x = Rep1, y=Rep2)) + geom_point() + facet_wrap(~scale)


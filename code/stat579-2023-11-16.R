# homework? - extension needed?
# project - does everyone have a team?
# presentation - sign up!
library(tidyverse)
data(french_fries, package="reshape2")

ff_long <- french_fries %>%
  pivot_longer(cols = 5:9, names_to = "scale", values_to = "score")

ff_long %>%
  pivot_wider(names_from=rep, values_from = score,
              names_prefix = "Rep ") %>%
  ggplot(aes(x = `Rep 1`, y = `Rep 2`)) + geom_point() +
  facet_wrap(~scale)

############


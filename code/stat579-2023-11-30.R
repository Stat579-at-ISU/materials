# presentation - sign up!
# participation during presentations
# midterm - extra points
# report - follow the rubric
# submit to stages
# HH: will post a template report

#####
install.packages("Lahman")
library(Lahman)

library(help="Lahman") # version from April 16 2023

LahmanData

# For this your turn use the HallOfFame and People data from the Lahman package

# Identify all players who were inducted in the Hall of Fame in 2022, by filtering the People data for their player IDs.

roster_2022 <- HallOfFame %>% filter(yearID==2022)

roster_2022$playerID

People$playerID == roster_2022$playerID[1]

People %>% slice(which(playerID== roster_2022$playerID[3]))

# Load the Lahman package into your R session.

# Join (relevant pieces of) the People data set and the HallOfFame data.

bigger <- HallOfFame %>% left_join(People %>% select(playerID, nameFirst, nameLast, deathYear))
bigger %>% filter(yearID==2022)


# Find all Hall of Famers who were alive as of 2022.
bigger %>% filter(is.na(deathYear), inducted=="Y")

# Who was most often on the ballot and in the end did or did not get inducted

wallabies <- read.table("http://www.statsci.org/data/oz/wallaby.txt", sep="\t", header=TRUE)


wallabies %>%
  ggplot(aes(x = Age, y = Weight)) +
  geom_vline(xintercept = 175) +
  geom_vline(xintercept = 375) +
  geom_point(aes(colour=factor(Sex))) +
  geom_text(aes(label = Anim),
     #       colour="Purple",
            hjust = 0, nudge_x = 5,
            data = wallabies %>% filter(Age > 450, Weight < 30000)) +
  annotate("text", x = 500, y=10000, hjust=0, label="The growth initially is\nexponential, then tapers off.") +
  ggthemes::theme_fivethirtyeight()




?install.packages
# repos is the parameter specifying the repository/cran server

getOption("repos") # your default
# "https://cran.rstudio.com/"
options(repos = "https://mirror.las.iastate.edu/CRAN/")
getOption("repos")

install.packages("x3ptools")

install.packages("remotes")

library(help="remotes")
remotes::install_github("") # might use this for homework #2
#####

remotes::install_github("heike/classdata")
library(classdata)

help(fbi)
head(fbi)

dim(fbi)
length(fbi)

str(fbi)
length(fbi$state_abbr)
dim(fbi$state_abbr)

summary(fbi)

NA # is a constant
NA + 1
2*NA

# your turn: scatterplots

data(fbiwide, package = "classdata")

# Error in ggplot(data = fbiwide, aes(x = burglary, y = murder)) :
# could not find function "ggplot"
#
library(ggplot2)

ggplot(data = fbiwide, aes(x = burglary, y = homicide)) +
  geom_point(alpha=0.01)

ggplot(data = fbiwide, aes(x = burglary, y = homicide)) +
  geom_point()

ggplot(data = fbiwide, aes(x = burglary, y = homicide)) +
  geom_point(aes(colour=state_abbr=="CA"))


cor(fbiwide$burglary, fbiwide$homicide)


ggplot(data = fbiwide, aes(x = log(burglary), y = log(homicide))) +
  geom_point()
ggplot(data = fbiwide, aes(x = log(burglary),
                           y = log(motor_vehicle_theft))) +
  geom_point()

library(magrittr)
library(dplyr)
library(tidyverse)

fbiwide %>%
  ggplot(aes(x = burglary, y=motor_vehicle_theft)) %>%
  geom_point()
# throws a useful error message

fbiwide |>
  ggplot(aes(x = burglary, y=motor_vehicle_theft)) +
  geom_point()


fbiwide |>
  ggplot(aes(x = burglary, y=motor_vehicle_theft,
             colour=state)) +
  geom_point()


fbiwide |>
  ggplot(aes(x = population, y=homicide)) +
  geom_point(aes(color = state))

library(plotly)
#install.packages("plotly")
ggplotly()



fbiwide |>
  ggplot(aes(x = state_abbr)) +
  geom_bar()

fbiwide |>
  ggplot(aes(x = population)) +
  geom_histogram()




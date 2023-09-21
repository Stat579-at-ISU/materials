fbiwide
# Error: object 'fbiwide' not found

library(classdata)
fbiwide

str(fbiwide)
# dir() function that lists all elements of the current working directory

fbiwide$state_id

library(ggplot2)
ggplot(data = fbiwide, aes(x = burglary, y = homicide)) +
  geom_point()

ggplot(data = fbiwide, aes(x = burglary, y = homicide)) +
  geom_point(alpha=0.1)

# mapping variable to the alpha transparency
ggplot(data = fbiwide, aes(x = burglary, y = homicide, alpha = 1/population)) +
  geom_point()

ggplot(data = fbiwide, aes(x = log10(burglary),
                           y = log10(motor_vehicle_theft))) +
  geom_point()


ggplot(data = fbiwide, aes(x = (burglary/population)*100000,
                           y = (motor_vehicle_theft/population)*100000)) +
  geom_point()

# too many colours
ggplot(data = fbiwide,
       aes(x = (burglary/population)*100000,
           y = (motor_vehicle_theft/population)*100000,
           color = state_abbr)
       ) +
  geom_point()

# install.packages("plotly")
library(plotly)
ggplotly() # shows the result in the Viewer panel


#####
# small multiples


ggplot(data = fbiwide,
       aes(x = population,
           y = motor_vehicle_theft,
           color = state_abbr)) +
  geom_point()

ggplot(data = fbiwide,
       aes(x = year,
           y = motor_vehicle_theft,
           color = state_abbr)) +
  geom_point()


ggplot(data = fbiwide,
       aes(x = year,
           y = motor_vehicle_theft,
           color = state_abbr)) +
  geom_point() +
  theme(legend.position = "none")

ggplot(data = fbiwide,
       aes(x = year,
           y = motor_vehicle_theft/population,
           color = state_abbr)) +
  geom_point() +
  theme(legend.position = "none")
# about 90000 people in Ames
ggplot(data = fbiwide,
       aes(x = year,
           y = motor_vehicle_theft/population*90000,
           color = state_abbr)) +
  geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~state, ncol=10)

ggplot(data = fbiwide,
       aes(x = year,
           y = motor_vehicle_theft/population*90000,
           color = state_abbr)) +
  geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~state, ncol=10, scales="free_y")

########

ggplot(data = fbiwide,
       aes(x = state_abbr,
           y = motor_vehicle_theft/population
           )) +
  geom_boxplot()


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


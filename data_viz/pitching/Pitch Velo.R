######################################################################################
######################################################################################
####################### Look at distribution of pitch velocity #######################
######################################################################################
######################################################################################

# Load the packages
library(dplyr) # For data selection
library(ggplot2) # For visualizing the data
library(ggthemes)

# First let us look at one single year
statcast %>% # Select the data (where statcast is the full statcast data from baseball savant)
  # Filter data by selecting the pitcher and removing missing pitch movements
  filter(player_name == "Gerrit Cole" & !is.na(pfx_x) & !is.na(pfx_x)) %>%
  # Filter by year (this example we are doing one year to compare velocity, select individual year)
  filter(game_year == 2020 & is.na(release_speed)) %>%
  ggplot(aes(x = release_speed, fill = as.factor(pitch_name))) +
  # Geom density will provide the distribution of pitch velocity from the pitcher
  geom_density(alpha = 0.50) + # Adjust the alpha level
  # Add lebels
  labs(title = "Pitch Velocity", # Title
       subtitle = "Distribution",
       x = "Release Speed (MPH)", # x-axis
       y = "Density", # y-axis
       caption = "Data: Baseball Savant\nData Visualization: TDK Baseball",
       fill = "Pitch Name") +
  theme_bw() +
  theme(text = element_text(size = 15))

# Now let us look at multiple years to compare how pitches look overtime
statcast %>% # Select the data (where statcast is the full statcast data from baseball savant)
  # Filter data by selecting the pitcher and removing missing pitch movements
  filter(player_name == "Gerrit Cole" & !is.na(pfx_x) & !is.na(pfx_x)) %>%
  # Filter by year (this example we are doing multiple years to compare each pitch type)
  filter(game_year >= 2017 & game_year <= 2020 & is.na(release_speed)) %>%
  ggplot(aes(x = release_speed, fill = as.factor(game_year))) +
  # Geom density will provide the distribution of pitch velocity from the pitcher
  geom_density(alpha = 0.50) + # Adjust the alpha level
  # Add labels
  labs(title = "Pitch Velocity",
       subtitle = "Distribution",
       x = "Release Speed (MPH)",
       y = "Density",
       caption = "Data: Baseball Savant\nData Visualization: TDK Baseball",
       fill = "Game Year") +
  theme_bw() +
  # Wrap by each pitch name (i.e. get the pitches in their own box) and make three columns
  facet_wrap(~ pitch_name, ncol = 3) +
  theme(text = element_text(size = 15))
######################################################################################
######################################################################################
###################### Look at moving average of pitch velocity ######################
######################################################################################
######################################################################################

# Load the packages
library(dplyr) # For data selection
library(ggplot2) # For visualizing the data
library(ggthemes)
library(zoo) # For moving average

statcast %>% # Use the data set
  select(game_year, release_speed, game_date, pitch_number, pitch_name, at_bat_number) %>% # select the variables
  filter(!is.na(pitch_name) & !is.na(release_speed)) %>% # Filter out where the pitch name and speed are empty
  filter(player_name == "Mike Foltynewicz") %>% # Select which player to use
  # We want to sort by date, at bat and pitch number to create the rolling average
  arrange(game_date, at_bat_number, pitch_number) %>%
  group_by(pitch_name, game_year) %>% # Group by pitch name and each year
  # Create a moving average of velocity (this example uses a 15-pitch moving average)
  mutate(release_speed.ma = rollapply(release_speed, 15, mean, align = 'right', fill = NA)) %>%
  ggplot(aes(x = game_year, y = velo, group = pitch_name, color = pitch_name)) + # Set the plot
  geom_line(size = 1.25) + # Size of line
  labs(title = "Mike Foltynewicz Velocity",
       subtitle = "Average by Year",
       x = "Game Year",
       y = "Release Speed",
       caption = "Data: Baseball Savant\nData Visualization: @piRatesanalysis",
       color = "Pitch Name") +
  theme_minimal() +
  theme(text = element_text(size = 20))
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

statcast %>% # Use the data (where statcast is the full statcast data from baseball savant)
  select(game_year, release_speed, game_date, pitch_number, pitch_name, at_bat_number) %>% # select the variables
  filter(!is.na(pitch_name) & !is.na(release_speed)) %>% # Filter out where the pitch name and speed are empty
  filter(player_name == "Mike Foltynewicz") %>% # Select which player to use
  # Filter by year (this example it is commented out but could be used for individual season)
  # If you want one year, make it game_year == "2020"
  # filter(game_year >= 2019 & game_year <= 2020)
  # We want to sort by date, at bat and pitch number to create the rolling average
  arrange(game_date, at_bat_number, pitch_number) %>%
  group_by(pitch_name, game_year) %>% # Group by pitch name and each year
  # Create a moving average of velocity (this example uses a 15-pitch moving average)
  mutate(release_speed.ma = rollapply(release_speed, 15, mean, align = 'right', fill = NA)) %>%
  ggplot(aes(x = game_year, y = velo, group = pitch_name, color = pitch_name)) + # Set the plot
  geom_line(size = 1.25) + # Size of line
  # Scale the colors of the pitch type
  # Red = fastball, blue = changeup, purple = curveball, green = cutter, yellow = sinker/2-seam, orange = slider
  # You can pick your own colors, these are just what I use
  scale_color_manual(labels = c("4-Seam Fastball", "Changeup", "Curveball", "Sinker", "Slider"), # These are the
                                                                            # pitches that Foltynewicz throws
                     values = c("red", "blue", "purple", "yellow", "orange")) + # Corresponding colors
  labs(title = "Mike Foltynewicz Velocity",
       subtitle = "Average by Year",
       x = "Game Year",
       y = "Release Speed",
       caption = "Data: Baseball Savant\nData Visualization: TDK Baseball",
       color = "Pitch Name") +
  theme_bw() +
  theme(text = element_text(size = 20))

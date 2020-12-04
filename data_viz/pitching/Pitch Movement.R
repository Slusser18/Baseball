######################################################################################
######################################################################################
######################### Look at pitch movement of pitches ##########################
######################################################################################
######################################################################################

# Load the packages
library(dplyr) # For data selection
library(ggplot2) # For visualizing the data
library(ggthemes)

statcast %>% # Select the data (where statcast is the full statcast data from baseball savant)
  # Filter data by selecting the pitcher and removing missing pitch movements
  filter(player_name == "Gerrit Cole" & !is.na(pfx_x) & !is.na(pfx_x)) %>%
  # Filter by year (this example it is commented out but could be used for individual season)
  # If you want one year, make it game_year == "2020"
  # filter(game_year >= 2019 & game_year <= 2020)
  ggplot(aes(x = pfx_x*12, y = pfx_z*12, color = pitch_name)) + # Multiply by 12 for inches
  geom_point() + # Size of point
  # Turn off the jitter; allow for overlapping points
  geom_jitter(position = position_jitter(width = NULL, height = NULL)) + 
  geom_vline(xintercept = 0) + # Add  0 line for reference
  geom_hline(yintercept = 0) + # Add 0 line for reference
  # Scale the colors of the pitch type
  # Red = fastball, blue = changeup, purple = curveball, green = cutter, yellow = sinker, orange = slider
  # You can pick your own colors, these are just what I use
  scale_color_manual(labels = c("4-Seam Fastball", "Changeup", "Knuckle Curve", "Slider"), # These are the
                     # pitches that Cole throws
                     values = c("red", "blue", "purple", "orange")) + # Corresponding colors
  # Add labels
  labs(title = "Pitch Movement",
       subtitle = "Catcher's POV",
       x = "Horizontal Movement (Inches)",
       y = "Vertical Movement (Inches)",
       caption = "Data: Baseball Savant\nData Visualization: TDK Baseball",
       color = "Pitch Name") +
  theme_bw() +
  theme(text = element_text(size = 15))

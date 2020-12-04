######################################################################################
######################################################################################
######################## Look at location of the strike zone #########################
######################################################################################
######################################################################################


# Load the packages
library(dplyr) # For data selection
library(ggplot2) # For visualizing the data
library(ggthemes)

statcast %>% # Select the data (where statcast is the full statcast data from baseball savant)
  # Filter data by selecting the pitcher and removing missing pitch names and pitch locations
  filter(player_name == "Mitch Keller" & !is.na(pitch_name) & !is.na(pfx_x) & !is.na(pfx_z)) %>%
  # Filter by year (this example it is commented out but could be used for individual season)
  # If you want one year, make it game_year = "2020"
  # filter(game_year >= 2019 & game_year <= 2020)
  group_by(pitch_name) %>% # Group by pitch type and arm for average
  ggplot(aes(x = plate_x, y = plate_z)) + # This is the pitch location for horizontal and vertical location
  # We want to have a stat density for the strikezone
  # But we want it to by pitch type so make nlevel
  stat_density2d(aes(fill = stat(nlevel)), geom = "polygon") +
  # Define the colors as the red (highest), yellow (middle) and green (lowest) palette
  scale_fill_distiller(palette = 'RdYlGn') +
  scale_x_continuous(limits = c(-3,3)) + # Scale the x axis
  scale_y_continuous(limits = c(-1, 6)) + # Scale the y axis
  # Adjust the labels
  labs(title = "Pitch Location",
       subtitle = "Catcher POV",
       caption = "Data: Baseball Savant\nData Visualization: TDK Baseball",
       x = "Horizontal Pitch Location",
       y = "Height from Ground") +
  theme_bw() +
  # Add the strikezone
  # Define the zone based on the horizontal distance of the plate and the vertical distance
  # Make it a black line with no fill
  geom_rect(mapping=aes(xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5), color = "black", alpha = 0) +
  # Wrap it by the pitch type in three columns (i.e. each pitch has it's own column)
  facet_wrap(~pitch_name, ncol = 3) +
  theme(text = element_text(size = 15),
        panel.spacing = unit(1.25, "lines"))
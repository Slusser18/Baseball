#################################################################################
##################### MLB estimate batted ball profile data #####################
############################### Comparing Ranges ################################
#################################################################################

# Load packages
library(readr) # Read in the data
library(dplyr) # Select data
library(ggplot2) # Data visualization
library(ggthemes)
library(scales)
library(baseballr)
library(GeomMLBStadiums) # View over MLB ballpark
library(patchwork) # Combine visualization

# Look at a player's density of location of where they fielded the ball
thirdBase %>% # This is a subset of the statcast data where a groundball was hit between -45º and -23.3º
  # For more information: http://tdkbaseball.com/evaluating-infield-defense-data-visualization-and-creating-a-model/
  # Select our third baseman, the game year and balls in which the player converted to an out
  filter((mlb_name == "Colin Moran" & game_year >= 2018 & Hit == "Out")) %>%
  ggplot(aes(x = hc_x, y = hc_y)) + # Plot the location of the batted ball
  stat_density2d(aes(fill = stat(nlevel), group = hit_location), geom = "polygon") +
  # Color is lightskyblue1 is Low and darkred is High (Red)
  scale_fill_gradient(low = "lightskyblue1", high = "darkred") +
  # Use the GeomMLBStadiums package to overlay on stadium
  geom_spraychart(stadium_ids = "pirates", # Since using a Pirate, use  PNC Park (team = pirates)
                  stadium_segments = "all",
                  size = 1, alpha = 0) +
  scale_y_reverse() + # flip the axis to get scaled properly
  # Add labels
  labs(title = "Colin Moran Infield Defense: As a Pittsburgh Pirate",
       subtitle = "Groundball Outs between -45º and -23.3º",
       x = "Horizontal location",
       y = "Vertical Location",
       color = "Name",
       caption = "Data: Baseball Savant\nData Visualization: TDK Baseball") +
  # Adjust theme
  theme_bw() +
  # View by Game Year over three columns (since we only are looking at 3 years. If 2 years or 4 years, 2 columns)
  facet_wrap(~ game_year, ncol = 3) +
  theme(text = element_text(size = 15), # Increase size of text
        aspect.ratio = 1, # Make facets square
        panel.spacing = unit(2, "lines")) # Increase facets between players

####################################################################################################
####################################################################################################
######################## Visualizing Spin Rotation Direction Using Statcast ########################
####################################################################################################
####################################################################################################


##### Load the packages
library(readr) # To read in dataset
library(dplyr) # To work with dataset
library(ggplot2) # Plot the data
library(ggthemes) # Themes for ggplot2

##### Read in the dataset
Statcast_2020 <- read_csv("Downloads/Statcast_2020.csv") # Full Statcast
spin_direction_pbp <- read_csv("Desktop/spin_direction_pbp.csv") # Spin Direction (Bill Petti)

# Merge the two files
# Common variables of batter, pitcher, game id, pitch number, inning
# Create the file name Statcast_2020_spin_direction
Statcast_2020_spin_direction <- merge(Statcast_2020, spin_direction_pbp,
                                      by = c('batter','pitcher','game_pk','pitch_number','inning'))

# Select the important variables that we need (this selects all the important pitch characteristic variables)
# Filter only to our pitcher (Gerrit Cole) and remove missing pitch names
# Save as "Cole"
# Use dplyr
Cole <- Statcast_2020_spin_direction %>%
  filter(!is.na(pitch_name) & player_name == "Gerrit Cole") %>% # Remove missing pitch names and
                                                                # Select Gerrit Cole as our pitcher
  select(player_name, pitch_name, release_speed, release_spin_rate, release_spin_direction,
         release_extension, release_pos_x, release_pos_z, release_pos_y, pfx_x, pfx_z, plate_x,
         plate_z, vx0, vy0, vz0, ax, ay, az) # Select the pitch characteristic

# We also want the averages by pitch name
# Again use dplyr
Cole <- Cole %>%
  group_by(pitch_name) %>%
  summarise(release_spin_direction = mean(release_spin_direction),
            count = n()) %>%
  mutate(freq = count / n())

##### Now visualize the spin rotation direction
# Use ggplot2
ggplot(Cole, aes(x = release_spin_direction, y = freq, color = pitch_name)) + # This makes length of arrow the
                                                                 # frequency of which the pitch was thrown
  coord_polar(start = pi, direction = 1) + # Use polar coordinates and start at pi (180?? at top) and go clockwise
  scale_x_continuous(breaks = seq(0, 360, by = 30), expand = c(0,0), lim = c(0, 360)) + # Break the circle up
                                                                           # by 30?? and label those points
  # Create the arrow on the polar chart with the length being frequency
  # We are making the arrow length the frequency but to see the spin direction, make yend = 1
  geom_segment(aes(y = 0, xend = release_spin_direction, yend = freq, color = pitch_name), # Cole by pitch name
               arrow = arrow(length = unit(0.3,"cm")), size = 1.5) + # Make the line thicker
  # Scale the colors of the pitch type
  # Red = fastball, blue = changeup, purple = curveball, green = cutter, yellow = sinker, orange = slider
  # You can pick your own colors, these are just what I use
  scale_color_manual(labels = c("4-Seam Fastball", "Changeup", "Knuckle Curve", "Slider"), # These are the
                                                                            # pitches that Cole throws
                     values = c("red", "blue", "purple", "orange")) + # Corresponding colors
  theme_bw() + # Make theme black and white
  # Add the labels
  labs(title = "Gerrit Cole Average Spin Direction", # Title
       subtitle = "2020 Season\nPitcher PoV", # Subtitle
       caption = "Data: Baseball Savant and Bill Petti\nData Visualization: TDK Baseball", # Caption
                                      # Always make sure to cite data source
                                      # Baseball Savant provides pitches and Bill Petti provided spin direction
       x = "Average Release Spin Direction", # Name the x-axis (We're removing the y so doesn't matter)
       color = "Pitch Name") + # Name the legend title
  # Adjust the theme elements
  theme(axis.title.y = element_blank(), # Remove the y-axis title
        axis.text.y = element_blank(), # Remove the y-axis text
        axis.ticks.y = element_blank(), # Remove the y-axis ticks
        text = element_text(size = 15)) # Increase the text size

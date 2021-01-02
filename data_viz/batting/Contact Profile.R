######################################################################
######################################################################
########################## Contact Profile ###########################
######################################################################
######################################################################

## This data visualization recreates the radial chart found on Baseball Savant
## For instance Mike Trout:
## https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2020%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&batters_lookup%5B%5D=545361&hfFlag=&hfBBT=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0#results_radialChart_name_545361_
## Instead, we will use hit probability to define the colors compared to savant using outcome as color
## Use Dr. Jim Albert's hit probability model:
## https://baseballwithr.wordpress.com/2018/01/15/chance-of-hit-as-function-of-launch-angle-exit-velocity-and-spray-angle/


## Load the packages
library(dplyr) # To work with dataset
library(ggplot2) # Plot the data
library(ggthemes) # Themes for ggplot2

## First let's define the outcome as hit (1) or out (0)
## We'll use this to illustrate how "lucky" a batter is
BIP$Outcome <- ifelse(BIP$hit == 1, "Hit", "Out")

## Create the radial chart
BIP %>% # Use the balls in play dataset (This is calculated from using the hit probability model data)
  ## Select the player we want and remove the missing launch angle and launch speed
  ## For this example, Bryce Harper
  filter(player_name == "Bryce Harper" & !is.na(as.numeric(launch_angle)) &
           !is.na(as.numeric(launch_speed))) %>%
  # Can filter by year and if a switch hitter by which side of the plate (variable is "stand")
  # Data goes back to 2015, but for this example since 2019
  filter(game_year >= 2019) %>%
  # Define x as the launch angle and y as the launch speed (exit velocity)
  ggplot(aes(x = as.numeric(launch_angle), y = as.numeric(launch_speed))) +
  # Use polar coordinates and start at pi/2 (90º) and go counterclockwise
  coord_polar(start = pi/2, direction = -1) +
  # Break the circle up by 30º intervals
  # Only go from -90º and 90º as a batted ball is constrained (directly up or directly down)
  scale_x_continuous(breaks = seq(-90, 90, by = 30), expand = c(0,0), lim = c(-180, 180)) +
  # Plot the points and color by the hit probability and shape by the outcome
  geom_point(aes(y = as.numeric(launch_speed), x = as.numeric(launch_angle), color = hitProb,
                 shape = as.factor(Outcome))) +
  # Scale from blue to red
  scale_color_gradient2(midpoint = 0.5, low = "blue", mid = "white",
                        high = "red", space = "Lab") +
  theme_bw() + # Make theme black and white
  # Add the labels
  labs(title = "Bryce Harper Batter Profile", # Title (Player Name)
       subtitle = "2019-2020 Seasons", # Subtitle (Years)
       caption = "Data: Baseball Savant\nHit Probability Model: Dr. Jim Albert\nData Visualization: TDK Baseball", # Caption
       # Always make sure to cite data source
       # Baseball Savant provides data and Dr. Jim Albert the hit probability model
       x = "Launch Angle (Degrees)", # Name the x-axis
       y = "Exit Velocity (MPH)", # Name the y-axis
       shape = "Outcome", # Name the legend title
       color = "Hit Probability") + # Name the legend title
  # Wrap by game year (Optional; best if comparing players or multiple years)
  facet_wrap(~game_year, ncol = 2) +
  theme(text = element_text(size = 15)) # Increase the text size

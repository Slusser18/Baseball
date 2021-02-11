########################################################################
########################################################################
#################### Scouting grade of max exit velo ###################
########################################################################
########################################################################

## Load packages
library(dplyr) # Data manipulation
library(ggplot2) # Data viz
library(ggthemes) # Plot theme
library(rstatix) # Quick stats

## Use batted balls dataset (full statcast data from 2015-2020)
## Make launch speed variable numeric
battedBalls$launch_speed <- as.numeric(battedBalls$launch_speed)
battedBalls <- battedBalls %>% filter(launch_speed != "null")

## Look at max exit velocity
## View to see distribution
battedBalls %>% 
  select(player_name, game_year, launch_speed) %>%   # Select variables needed
  group_by(player_name, game_year) %>%   # Group by name and year
  add_count(player_name) %>%   # Add a count of player names by year
  filter(n >= 50) %>%   # Filter to at least 25 batted balls in a season
  slice(which.max(launch_speed)) %>%   # Select only the max value of each player
  ggplot(aes(x = launch_speed)) +
    # Make a density plot
    geom_density(fill = "#515155", alpha = 0.5) +
    # Add mean and median line
    geom_vline(aes(xintercept = mean(launch_speed)),
             color = "black") +
   geom_vline(aes(xintercept = median(launch_speed)),
             color = "red") +
    theme_bw() +
    labs(title = "Distribution of Max Exit Velocity: Min 50 Batted Balls",
         subtitle = "Years 2015-2020",
         x = "Exit Velocity",
         y = "Density",
         fill = "Year",
         caption = "Data: Baseball Savant\nRed Line = Median || Black Line = Mean\nData Visualization: TDK Baseball") +
  theme(text = element_text(size = 15))

## Create a z-score for launch speed
maxEV <- battedBalls %>% 
  select(player_name, game_year, launch_speed) %>% # Select variables needed
  group_by(player_name, game_year) %>%  # Group by name and year
  add_count(player_name) %>% # Add a count of player names by year
  filter(n >= 50) %>% # Filter to at least 50 batted balls in a season (Remove essentially pitchers)
  slice(which.max(launch_speed)) %>%  # Select only the max value of each player
  ungroup() %>% # Ungroup to get z-score by player
  mutate(z_score = round(scale(launch_speed), 2)) # Calculate z-score

## Get mean and SD
## This will be used for calculating z-score on grade based on inputs
maxEVMean <-battedBalls %>% 
  select(player_name, game_year, launch_speed) %>% # Select variables needed
  group_by(player_name, game_year) %>%  # Group by name and year
  add_count(player_name) %>% # Add a count of player names by year
  filter(n >= 50) %>% # Filter to at least 50 batted balls in a season (Remove essentially pitchers)
  slice(which.max(launch_speed)) %>%  # Select only the max value of each player
  ungroup() %>% # Ungroup to get z-score by player
  summarise(mean = mean(launch_speed),  # Calculate mean
            sd = sd(launch_speed)) # Calculate sd

## Calculate the PV of power grade
maxEV$PV <- cut(maxEV$z_score,
                breaks = c(-Inf, -3, -2, -1, -0.5, 0.5, 1, 2, 2.5, Inf),
                labels = c(20, 30, 40, 45, 50, 55, 60, 70, 80))

## Look at top quartile of exit velocity
battedBalls %>% 
  select(player_name, game_year, launch_speed) %>% # Select variables needed
  group_by(player_name, game_year) %>%  # Group by name and year
  add_count(player_name) %>% # Add a count of player names by year
  filter(n >= 50) %>% # Filter to at least 50 batted balls in a season (Remove essentially pitchers)
  mutate(quantile_rank = ntile(desc(launch_speed), 4)) %>% # Calculate the quartile of each batted ball
  filter(quantile_rank == 1) %>% # Filter to just the top quartile
  summarise(launch_speed = mean(launch_speed)) %>% # Get the average of top quartile
  ggplot(aes(x = launch_speed)) +
  # Make a density plot
  geom_density(fill = "#515155", alpha = 0.5) +
  # Add mean and median line
  geom_vline(aes(xintercept = mean(launch_speed)),
             color = "black") +
  geom_vline(aes(xintercept = median(launch_speed)),
             color = "red") +
  theme_bw() +
  labs(title = "Distribution of Average Top Quartile In Exit Velocity: Min 50 Batted Balls",
       subtitle = "Years 2015-2020",
       x = "Exit Velocity",
       y = "Density",
       fill = "Year",
       caption = "Data: Baseball Savant\nRed Line = Median || Black Line = Mean\nData Visualization: TDK Baseball") +
  theme(text = element_text(size = 15))

## Create a z-score for launch speed
maxEVquartile <- battedBalls %>% 
  select(player_name, game_year, launch_speed) %>% # Select variables needed
  group_by(player_name, game_year) %>%  # Group by name and year
  add_count(player_name) %>% # Add a count of player names by year
  filter(n >= 50) %>% # Filter to at least 50 batted balls in a season (Remove essentially pitchers)
  mutate(quantile_rank = ntile(desc(launch_speed), 4)) %>% # Calculate the quartile
  filter(quantile_rank == 1) %>% # Filter to just top quartile
  summarise(launch_speed = mean(launch_speed)) %>% # Average the top quartile
  ungroup() %>% # Ungroup to get z-score by player
  mutate(z_score = round(scale(launch_speed), 2)) # Calculate z-score

## Calculate the PV of power grade
maxEVquartile$PV <- cut(maxEVquartile$z_score,
                        breaks = c(-Inf, -3, -2, -1, -0.5, 0.5, 1, 2, 2.5, Inf),
                        labels = c(20, 30, 40, 45, 50, 55, 60, 70, 80))

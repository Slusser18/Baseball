##########################################################################################
##########################################################################################
######################################## DRA Sims ########################################
##########################################################################################
##########################################################################################

## Load in packages
library(readr) # Read in the baseball prospectus data
library(readxl) # Read in the teams data
library(tidyverse) # Manipulate data
library(ggplot2) # For data visualization
library(ggthemes) # For data theme
library(ggridges) # For ridge plot
library(scales) # For scaling axis
library(teamcolors) # For team colors

## Read in data
## Comes from baseball prospectus
## https://www.baseballprospectus.com/leaders/hitting/hitting-standard-by-season?year=2020&lg=all&team=all&lvl=mlb&min=60
## Also load in the teams data to merge with drc
drc <- read_csv("/Users/davidslusser/Downloads/hitting-standard-by-season-0-mlb-2020.csv")
drc$year <- "2020"
teams <- read_excel("/Users/davidslusser/Desktop/teams.xlsx", 
                    sheet = "bpro") # Use BPro for team colors

## Now we want to create a simulation of DRC
## We know the DRC and DRC SD
## 10,000 simulations
numsims = 10000 # Number of simulations

## Create a new DRA table where it is the player's name, id, drc, and drc sd
drc_table = data.frame(name = drc$name, # Player name
                       playerid = drc$playerid, # Player ID
                       team = drc$team, # Team
                       year = drc$year, # Year
                       drc = drc$drc_plus, # Player DRA
                       drc_sd = drc$drc_sd) # Player SD

## Now simulate the player's DRA
drc_sim = bind_rows(replicate(numsims, drc_table, simplify = F), .id = 'simnumber')

## Aggregate the simulation
drc_sim = drc_sim%>%
  group_by(playerid, name, team, year)%>% # Group by player id
  # Add the simulated values
  mutate(drc_sim = round(rnorm(n(), unique(drc)[1], unique(drc_sd)[1]), 2))

## Now merge the teams on for the team color package and merge the team colors on as well
drc_sim <- merge(drc_sim, teams, by = "team")
drc_sim <- merge(drc_sim, teamcolors, by.x = "team_name", by.y = "name")

## Plot DRA distribution
## Set the color palette
league_pal("mlb", 1)

## Plot distribution
drc_sim %>%
  # Filter hitters
  filter(name == "Bryce Harper" | name == "Mike Trout" | name == "Juan Soto") %>%
  arrange(mean(drc_sim)) %>%
  # X-axis the dra, y-axis the name
  # We want the color to be the player (name) but use team color
  ggplot(aes(x = drc_sim, y = name, color = team_name, fill = team_name)) +
  # Density plot
  geom_density_ridges(quantile_lines = TRUE, # Add median line based on dra
                      quantile_fun = function(drc_sim,...)mean(drc_sim),
                      color = "black",
                      scale = 0.8,
                      alpha = 0.75) +
  # Add text to the data viz (DRA value)
  geom_text(drc_sim %>%
              group_by(name, team_name) %>% # Group by name and team
              # Filter to hitters
              filter(name == "Bryce Harper" | name == "Mike Trout" | name == "Juan Soto") %>%
              summarise(drc_sim = mean(drc_sim)) %>%
              arrange(drc_sim),
            # Set mapping
            mapping = aes(label=sprintf("%1.0f", drc_sim), # Label to 2 decimal places
                          color = team_name), # Change color
            position=position_nudge(y = -0.05),
            size=3.5) + # Increase the size
  # Add text to the data viz ("DRA")
  geom_text(drc_sim %>%
              group_by(name, team_name) %>% # Group by name and team
              # Filter to hitters
              filter(name == "Bryce Harper" | name == "Mike Trout" | name == "Juan Soto") %>%
              summarise(drc_sim = mean(drc_sim)) %>%
              arrange(drc_sim),
            # Set mapping
            mapping = aes(label=sprintf("DRC+: "), color = team_name), 
            position=position_nudge(y = -0.05, x = -5), size = 3.5) + # Increase the size
  # Remove legends
  scale_fill_teams(name = "Team", guide = FALSE) + 
  scale_color_teams(name = "Team", guide = FALSE) + 
  # Add labels
  labs(title = "2020 Hitters", # Title
       subtitle = "10,000 Simulations", # Subtitle
       x = "DRC+", # X-axis
       y = "Player", # Y-axis
       caption = "Data: Baseball Prospectus\nData Visualization: TDK Baseball") + # Caption and cite source
  theme_minimal() + # Change the theme to minimal
  theme(text = element_text(size = 15)) # Increase text size
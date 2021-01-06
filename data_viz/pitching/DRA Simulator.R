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
## Also load in the teams data to merge with dra
dra <- read_csv("/Users/davidslusser/Downloads/dra.csv")
dra$year <- "2020"
teams <- read_excel("Desktop/teams.xlsx", 
                    sheet = "bpro") # Use BPro for team colors

## Now we want to create a simulation of dra
## We know the dra and dra SD
## 10,000 simulations
numsims = 10000 # Number of simulations

## Create a new DRA table where it is the player's name, id, dra, and dra sd
dra_table = data.frame(name = dra$name, # Player name
                       playerid = dra$playerid, # Player ID
                       team = dra$team, # Team
                       year = dra$year, # Year
                       dra= dra$dra, # Player DRA
                       dra_sd = dra$dra_sd) # Player SD

## Now simulate the player's DRA
dra_sim = bind_rows(replicate(numsims, dra_table, simplify = F), .id = 'simnumber')

## Aggregate the simulation
dra_sim = dra_sim%>%
  group_by(playerid, name, team, year)%>% # Group by player id
  # Add the simulated values
  mutate(dra_sim = round(rnorm(n(), unique(dra)[1], unique(dra_sd)[1]), 2))

## Now merge the teams on for the team color package and merge the team colors on as well
dra_sim <- merge(dra_sim, teams, by = "team")
dra_sim <- merge(dra_sim, teamcolors, by.x = "team_name", by.y = "name")

## Plot DRA distribution
## Set the color palette
league_pal("mlb", 1)

## Plot distribution
dra_sim %>%
  # Filter players
  filter(name == "Joe Musgrove" | name == "Steven Brault" | name == "Mitch Keller") %>%
  arrange(mean(dra_sim)) %>%
  # X-axis the dra, y-axis the name
  # We want the color to be the player (name) but use team color
  ggplot(aes(x = dra_sim, y = name, color = team_name, fill = team_name)) +
  # Density plot
  geom_density_ridges(quantile_lines = TRUE, # Add median line based on dra
                      quantile_fun = function(dra_sim,...)mean(dra_sim),
                      color = "black",
                      scale = 0.8,
                      alpha = 0.75) +
  # Add 2 decimals
  scale_x_continuous(expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.01)) +
  # Add text to the data viz (DRA value)
  geom_text(dra_sim %>%
              group_by(name, team_name) %>% # Group by name and team
              # Filter to pitchers
              filter(name == "Joe Musgrove" | name == "Steven Brault" | name == "Mitch Keller") %>%
              summarise(dra_sim = mean(dra_sim)) %>%
              arrange(dra_sim),
            # Set mapping
            mapping = aes(label=sprintf("%1.2f", dra_sim), # Label to 2 decimal places
            color = team_name), # Change color
            position=position_nudge(y = -0.05),
            size=3.5) + # Increase the size
  # Add text to the data viz ("DRA")
  geom_text(dra_sim %>%
              group_by(name, team_name) %>% # Group by name and team
              # Filter to pitchers
              filter(name == "Joe Musgrove" | name == "Steven Brault" | name == "Mitch Keller") %>%
              summarise(dra_sim = mean(dra_sim)) %>%
              arrange(dra_sim),
            # Set mapping
            mapping = aes(label=sprintf("DRA: "), color = team_name), 
            position=position_nudge(y = -0.05, x = -0.25), size = 3.5) + # Increase the size
  # Remove legends
  scale_fill_teams(name = "Team", guide = FALSE) + 
  scale_color_teams(name = "Team", guide = FALSE) + 
  # Add labels
  labs(title = "2020 Pitchers", # Title
       subtitle = "10,000 Simulations", # Subtitle
       x = "DRA", # X-axis
       y = "Player", # Y-axis
       caption = "Data: Baseball Prospectus\nData Visualization: TDK Baseball") + # Caption and cite source
  theme_minimal() + # Change the theme to minimal
  theme(text = element_text(size = 15)) # Increase text size

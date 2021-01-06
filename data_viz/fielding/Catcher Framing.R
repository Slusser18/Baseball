#########################################################################################
#########################################################################################
#################################### Catcher Framing ####################################
#########################################################################################
#########################################################################################

## Load the packages
library(readr) # Read in the csv file from baseball prospectus
library(readxl) # Read in the xlsx file of team names
library(dplyr) # Manipulate the data
library(ggplot2) # Data visualization
library(ggthemes) # Change the theme
library(scales) # Change the y-axis
library(magrittr) # Chain commands
library(ggforestplot) # For plotting confidence intervals
library(teamcolors) # MLB team colors

## Load in the data and filter chances to greater than 1500
## For 2020, threshold is set lower for "starters" as season was short because of covid-19 pandemic
## Data link: https://www.baseballprospectus.com/leaders/catching/catcher-stats-full-season?year=2020&lg=all&team=all&lvl=mlb&min=qualified
## Also load in the teams data
BP <- read_csv("/Users/davidslusser/Desktop/catchers.csv") %>%
  filter(csaa_chances >= 1500) # This filter to 1500 framing chances

## Teams dataset
teams <- read_excel("Desktop/teams.xlsx", 
                    sheet = "bpro")

## We want by team color, so merge the files with the teams name
BP <- merge(BP, teams, by = "team")

## We want by team color, so merge the files with the teamcolors file
BP <- merge(BP, teamcolors, by.x = "team_name", by.y = "name")

## Reorder where highest csaa (Called strikes above average) is first
BP$name = with(BP, reorder(name, csaa))

## Plot the data
BP %>%
  # X-axis the csaa and y-axis is player name
  # We want the color to be the team color
  ggplot(aes(x = csaa, y = name, color = team_name)) +
    geom_point(stat = "identity",
             aes(size = csaa_chances), color = BP$primary) + # Make a point chart where the size is chances
    geom_errorbar(aes(xmin = csaa - csaa_sd, xmax = csaa + csaa_sd), # Add an error bar (+/- 1 SD)
                  color = BP$secondary) + # Make the error bar the team's secondary color
    scale_color_teams(name = "Team", guide = FALSE) +
    # Add striped background
    # Alternate gray and white
    geom_stripes(odd = "#33333333", even = "#00000000") +
    # Add labels
    labs(title = "2020 Pitch Framing Leaders", # Title
         subtitle = "Minimum 1500 CSAA Chances. +/- 1 SD", # Subtitle
         # Caption
         # Cite both data (Baseball Prospectus) and colors (TeamColors by Baumer and Matthews)
         caption = "Data: Baseball Prospectus\nColors: Team Colors Package in R (Ben Baumer)\nData Visualization: TDK Baseball",
         x = "Called Strikes Above Average", # X-axis
         y = "Name", # Y-axis
         size = "Framing Chances") + # Size
    theme_minimal() + # Change the theme to minimal
    theme(text = element_text(size = 15)) # Increase text size
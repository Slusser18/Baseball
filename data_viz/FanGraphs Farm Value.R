#######################################################################
#######################################################################
############## Use FanGraphs to examine MLB Farm Values ###############
#######################################################################
#######################################################################

## Load packages
library(readr) # Read in CSV
library(readxl) # Read in Excel
library(dplyr) # Data manipulation
library(ggplot2) # Plot the data
library(ggthemes) # Theme for plots
library(scales) # Scale the axis
library(teamcolors) # We want this for the hex colors in the chart

#######################################################################
########################## Data preperation ###########################
#######################################################################

## Load in the data
fg17 <- read_csv("/Volumes/LaCie/Baseball/Data/Prospects/FanGraphs Preseason/fg17.csv") # 2017 preseason
fg18 <- read_csv("/Volumes/LaCie/Baseball/Data/Prospects/FanGraphs Preseason/fg18.csv") # 2018 preseason
fg19 <- read_csv("/Volumes/LaCie/Baseball/Data/Prospects/FanGraphs Preseason/fg19.csv") # 2019 preseason
fg20 <- read_csv("/Volumes/LaCie/Baseball/Data/Prospects/FanGraphs Preseason/fg20.csv") # 2020 preseason

## Add year variables
fg17$Year = 2017
fg18$Year = 2018
fg19$Year = 2019
fg20$Year = 2020

## Merge the prospects together for one dataset
fgProspects <- rbind(fg17, fg18, fg19, fg20)

## Convert positions to pitchers and hitters
## In 2021, FG added "MIRP" and "SIRP" for multiple inning and short inning relief pitcher
fgProspects$Position <- ifelse(fgProspects$Pos == "RHP" | fgProspects$Pos == "LHP" |
                                 fgProspects$Pos == "MIRP" | fgProspects$Pos == "LIRP",
                               "Pitcher", "Hitter")

## Load in the trade value
## This research comes from Craig Edwards
## Top 100 (50 or better prospects): https://blogs.fangraphs.com/an-update-to-prospect-valuation/
## Not top 100: https://blogs.fangraphs.com/putting-a-dollar-value-on-prospects-outside-the-top-100/
## Driveline hybrid values: https://www.drivelinebaseball.com/2019/02/prospect-valuation-much-top-prospects-worth-professional-baseball-teams/
Value <- read_excel("/Volumes/LaCie/Baseball/Data/Prospects/Value.xlsx", 
                    sheet = "Long") # Load in prospect value


## Merge the datasets to generate one prospect dataset
prospects = merge(fgProspects[, c("FV", "Name", "Org", "Position", "Year")], 
                  Value[, c("FV", "Position", "Dollar Value")])

## Load the teams dataset
## We want this because of the hex colors
teams <- read_excel("~/Desktop/teams.xlsx")

## Merge onto the prospects dataset
## Common variable in prospects is org and in y is team
prospects = merge(prospects, teams, by.x = "Org", by.y = "team")

## Now merge the team colors onto the prospects data
prospects = merge(prospects, teamcolors, by = "name")

## Sum prospect value by team and year
prospectsTeam <- prospects %>%
  group_by(Org, name, Year, primary, secondary) %>% # Team and year but also the colors
  summarize(Players = n(), # Get number of prospects
                   Farm.Value = sum(`Dollar Value`), # Sum farm value
                   Per.Player = round(Farm.Value/Players, 2)) %>% # Get farm value per player
  # We now want to rank the teams by farm value
  ungroup() %>% # Ungroup by org and year
  group_by(Year) %>% # only group by year
  arrange(Year, desc(Farm.Value)) %>%  # Arrange by farm value
  mutate(rank = row_number()) # Rank each team in each year by their farm value


#######################################################################
######################### Data Visualization ##########################
#######################################################################
## Set the palette
league_pal("mlb", 2)

## Count of prospects by FV
prospects %>% # Use the prospects dataset
  filter(Org == "LAD") %>% # Filter to the team we want
  ggplot(aes(x = FV, color = name, fill = name)) + # X will be FV since making a histogram
  # Color and fill the bars by team color and make lighter
  geom_bar(alpha = 0.8, position = 'identity') +
  # Fill by team
  scale_fill_teams(1, guide = FALSE) + 
  # Color by team
  scale_color_teams(2, guide = FALSE) + 
  # Force to start at y = 0 max(Listed2$value)
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  # Add the count label
  geom_text(stat = 'count', aes(label = stat(count), vjust = -0.8),  color = "#000000") +
  # Add labels to graph
  labs(title = "Tampa Bay Rays Future Value Grades", # Title
       subtitle = "2017-2020 Preseason Ranking", # Subtitle
       caption = "Data: Fangraphs\nData Visualization: TDK Baseball", # Caption and citation
       x = "Future Value", # X-axis
       y = "Count") + # Y-axis
  theme_minimal() + # Change theme
  theme(text = element_text(size=15)) + # Increase size of text
  facet_wrap(~Year, ncol = 1) # Make columns of year

## Look at the total farm value per year 
prospectsTeam %>% # Use the prospectsTeam dataset
  filter(Org == "PIT") %>% # Filter to the organization
  # Graph the farm value. Could use Per.Player instead
  ggplot(aes(x = as.factor(Year), y = Farm.Value, group = Org, color = name, fill = name)) +
  # We want a line graph connecting the points
  geom_line(size = 2) + # Line graph with team color
  geom_point(size = 2) + # Make the points team color and fill
  # Fill by team
  scale_fill_teams(guide = FALSE) + 
  # Color by team
  scale_color_teams(guide = FALSE) + 
  # Force the y-axis to start at y = 0 and end at the max of farm value
  # Convert the labels to dollar format
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(prospectsTeam$Farm.Value) + 50),
                     labels = dollar) +
  # Add text at the (x, y) locations, label in dollar format
  # Increase the size and change the location
  geom_text(aes(as.factor(Year), Farm.Value, label = dollar(Farm.Value)),
            color = "#000000",
            size = 5, 
            hjust = 0.2, vjust = -1.2) +
  # Add labels
  labs(title = "Pittsburgh Pirates Farm System Value", # Title
       subtitle = "2017-2020 Preseason Ranking", # Subtitle
       caption = "Data: Fangraphs\nData Visualization: TDK Baseball", # Caption with citation of data
       x = "Season", # X-axis
       y = "Farm System Value") + # Y-axis
  theme_minimal() + # Set the theme
  theme(text = element_text(size=15)) # Increase size of text

## Look at how the farm system ranks in a given season
prospectsTeam %>% # Use the prospectsTeam dataset
  filter(Org == "BAL") %>% # Filter by team
  # Plot the year and the season rank
  ggplot(aes(x = as.factor(Year), y = rank, group = Org)) +
  geom_line(aes(color = name), size = 2) + # Line graph with team color
  geom_point(aes(fill = name), size = 2) + # Make the points team color and fill
  # Fill by team
  scale_fill_teams(2, guide = FALSE) + 
  # Color by team
  scale_color_teams(1, guide = FALSE) + 
  scale_y_reverse(expand = c(0, 0), limits = c(33, -1)) + # Flip so 1 is at the top
  # Add text to graph (the rank of a given year), increase the size, and change the position
  geom_text(aes(as.factor(Year), rank, label = rank), # Add text to graph
            color = "#000000", # Change color
            size = 5, # Increase size
            hjust = 0.1, vjust = 1.5) + # Change position
  # Add labels
  labs(title = "Baltimore Orioles Farm System Ranking", # Add title
       subtitle = "2017-2020 Preseason Ranking", # Add subtitle
       caption = "Data: Fangraphs\nData Visualization: TDK Baseball", # Add caption and cite data source
       x = "Season", # X-axis
       y = "Rank") + # Y-axis
  theme_minimal() + # Change the theme
  theme(text = element_text(size=15)) # Increase size of text
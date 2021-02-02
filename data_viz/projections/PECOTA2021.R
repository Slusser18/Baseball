################################################################################
################################################################################
############################ PECOTA 2021 PROJECTIONS ###########################
################################################################################
################################################################################

## Load the packages needed
library(readxl) # Load in the excel from Baseball Prospectus
library(dplyr) # For data manipulation
library(forcats) # For arranging y-axis
library(sqldf) # SQL
library(ggplot2) # For graphs
library(ggthemes) # For graph theme
library(scales) # Scale the axis
library(ggridges) # For ridge plots

################################################################################
################################### Hitters ####################################
################################################################################

## Load in the hitter data by percentile
P1H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                  sheet = "1")
P5H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                  sheet = "5")
P10H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "10")
P20H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "20")
P30H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "30")
P40H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "40")
P50H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "50")
P60H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "60")
P70H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "70")
P80H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "80")
P90H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "90")
P95H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "95")
P99H <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.hitters.xlsx", 
                   sheet = "99")

## Combine them into one file set for reference of one player
PECOTA2021_hitters <- rbind(P1H, P5H, P10H, P20H, P30H, P40H, P50H, P60H,
                            P70H, P80H, P90H, P95H, P99H)

## We now want to create a distribution
## We'll use DRC+
## 10,000 simulations
numsims = 10000 # Number of simulations

## We are going to want to calculate a standard deviation
## We do this by assuming the Central Limit Theorem (CLT)
## This gives us a normal distribution
## Thus since we know percentiles based on z-score, we can backtrack a SD
## z = (x - u) / o where u = mean and o = SD
## z for 10th percentile is -1.282
## -> o = (x-u) / -1.282 = SD
## We will use SQL for those
PECTOTA_drc <- sqldf('SELECT P10H.bpid AS "bpid",
                     P10H.name AS "name",
                     P10H.team AS "team",
                     P10H.drc_plus AS "drc_plus_10",
                     P50H.drc_plus AS "drc_plus_50",
                     (P10H.drc_plus - P50H.drc_plus) / -1.282 AS "drc_plus_sd"
                     FROM P10H
                     LEFT JOIN P50H ON P10H.bpid = P50H.bpid') 

## Create a new drc table where it is the player's name, id, drc, and drc sd
drc_table = data.frame(name = PECTOTA_drc$name, # Player name
                       bpid = PECTOTA_drc$bpid, # Player ID
                       team = PECTOTA_drc$team, # Team
                       drc_plus = PECTOTA_drc$drc_plus_50, # Player DRC
                       drc_sd = PECTOTA_drc$drc_plus_sd) # Player SD

## Now simulate the player's DRC
drc_sim = bind_rows(replicate(numsims, drc_table, simplify = F), .id = 'simnumber')

## Aggregate the simulation
drc_sim = drc_sim %>%
  group_by(bpid, name, team) %>% # Group by player id
  # Add the simulated values
  mutate(drc_sim = round(rnorm(n(), unique(drc_plus)[1], unique(drc_sd)[1]), 2))

## Plot DRC distribution
drc_sim %>%
  # Filter players to just the Pirates likely main contributors
  filter(name == "Jacob Stallings" | name == "Michael Perez" | name == "Colin Moran" |
           name == "Adam Frazier" | name == "Ke'Bryan Hayes" | name == "Cole Tucker" |
           name == "Erik Gonzalez" | name == "Kevin Newman" | name == "Bryan Reynolds" |
           name == "Anthony Alford" | name == "Gregory Polanco" | name == "Jared Oliva" |
           name == "Phillip Evans") %>%
  mutate(name = fct_reorder(name, drc_plus, .fun = 'median')) %>%
  # X-axis the drc, y-axis the name
  # We want the color to be the player (name) but use team color if comparing different teams
  ggplot(aes(x = drc_sim, y = reorder(name, drc_sim))) +
  # Density plot
  geom_density_ridges(quantile_lines = TRUE, # Add median line based on dra
                      quantile_fun = function(drc_sim,...)mean(drc_sim),
                      color = "#000000", # Black (Pirates colors)
                      fill = "#FDB827", # Gold (Pirates colors)
                      scale = 0.8,
                      alpha = 0.75) +
  # Add x = 100 (league average line)
  geom_vline(xintercept = 100, color = "red") + 
  # Add 2 decimals
  scale_x_continuous(expand = c(0, 0),
                     labels = scales::number_format(accuracy = 1)) +
  # Add labels
  labs(title = "2021 PECOTA Projections",
       subtitle = "Pittsburgh Pirates DRC+: 10,000 Simulations",
       x = "DRC+ Expected Range",
       y = "Name",
       caption = "Data: Baseball Prospectus\nData Visualization: TDK Baseball") +
  # Change theme
  theme_bw() +
  # Increase text size
  theme(text = element_text(size = 20))

################################################################################
################################## Pitchers ####################################
################################################################################

## Load in the hitter data by percentile
P1P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                  sheet = "1")
P5P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                  sheet = "5")
P10P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "10")
P20P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "20")
P30P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "30")
P40P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "40")
P50P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "50")
P60P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "60")
P70P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "70")
P80P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "80")
P90P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "90")
P95P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "95")
P99P <- read_excel("/Volumes/LaCie/Baseball/Data/PECOTA Projections/2021.pitchers.xlsx", 
                   sheet = "99")

## Combine them into one file set for reference of one player
PECOTA2021_pitchers <- rbind(P1P, P5P, P10P, P20P, P30P, P40P, P50P, P60P,
                             P70P, P80P, P90P, P95P, P99P)

## We now want to create a distribution
## We'll use DRC+
## 10,000 simulations
numsims = 10000 # Number of simulations

## We are going to want to calculate a standard deviation
## We do this by assuming the Central Limit Theorem (CLT)
## This gives us a normal distribution
## Thus since we know percentiles based on z-score, we can backtrack a SD
## z = (x - u) / o where u = mean and o = SD
## z for 10th percentile is -1.282
## -> o = (x-u) / -1.282 = SD
## We will use SQL for this
PECTOTA_dra <- sqldf('SELECT P10P.bpid AS "bpid",
                             P10P.name AS "name",
                             P10P.team AS "team",
                             P10P.dra AS "dra_10",
                             P50P.dra AS "dra_50",
                             Abs((P10P.dra - P50P.dra) / -1.282) AS "dra_sd"
                     FROM P10P
                     LEFT JOIN P50P ON P10P.bpid = P50P.bpid') 

## Create a new dra table where it is the player's name, id, dra, and dra sd
dra_table = data.frame(name = PECTOTA_dra$name, # Player name
                       bpid = PECTOTA_dra$bpid, # Player ID
                       team = PECTOTA_dra$team, # Team
                       dra = as.numeric(PECTOTA_dra$dra_50), # Player DRA
                       dra_sd = PECTOTA_dra$dra_sd) # Player SD

## Now simulate the player's DRA
dra_sim = bind_rows(replicate(numsims, dra_table, simplify = F), .id = 'simnumber')

## Aggregate the simulation
dra_sim = dra_sim %>%
  dplyr::group_by(bpid, name, team) %>% # Group by player id
  # Add the simulated values
  dplyr::mutate(dra_sim = round(rnorm(n(), unique(dra)[1], unique(dra_sd)[1]), 2))

## Plot DRC distribution
dra_sim %>%
  # Filter players to just the Pirates likely main contributors
  filter(name == "Mitch Keller" | name == "Wil Crowe" | name == "Chad Kuhl" |
           name == "Steven Brault" | name == "Cody Ponce" | name == "JT Brubaker") %>%
  mutate(name = fct_reorder(name, dra, .fun = 'median', .desc = FALSE)) %>%
  # X-axis the dra, y-axis the name
  # We want the color to be the player (name) but use team color
  ggplot(aes(x = dra_sim, y = reorder(name, desc(dra_sim)))) +
  # Density plot
  geom_density_ridges(quantile_lines = TRUE, # Add median line based on dra
                      quantile_fun = function(dra_sim,...)mean(dra_sim),
                      color = "#000000", # Black (Pirates colors)
                      fill = "#FDB827", # Gold (Pirates colors)
                      scale = 0.8,
                      alpha = 0.75) +
  # Add x = 4.85 (2020 RA9)
  geom_vline(xintercept = 4.85, color = "red") + 
  # Add 2 decimals
  scale_x_continuous(expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.01)) +
  # Add labels
  labs(title = "2021 PECOTA Projections",
       subtitle = "Pittsburgh Pirates DRA: 10,000 Simulations",
       x = "DRA Expected Range",
       y = "Name",
       caption = "Data: Baseball Prospectus\nData Visualization: TDK Baseball") +
  # Change theme
  theme_bw() +
  # Increase text size
  theme(text = element_text(size = 20))

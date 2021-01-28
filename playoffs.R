###################################################################################
###################################################################################
################################# Wins and Playoffs ###############################
###################################################################################
###################################################################################

## Load the packages
library(readxl) # Load in playoffs data
library(dplyr) # Used to extract columns in the data
library(ggplot2) # Used for plotting data
library(ggthemes) # Change theme of ggplot
library(scales) # Change the axis

## Load in the data
mlb_wins <- read_excel("Desktop/mlb_wins.xlsx")

## Create a logit model
## Determine probability of making playoffs by number of wins
## Data is 2012-2019
playoffs.glm <- glm(Playoffs ~ W, data = mlb_wins, family = binomial)

mlb_wins %>%
  ggplot(aes(x = W, y = Playoffs)) + 
    # Make points lighter, color black and increase size
    geom_point(alpha = .5, fill = "black", size = 2) +
    # Add the sigmoid and color gold with the standard error abr
    stat_smooth(method = "glm", se = TRUE, method.args = list(family = binomial),
                color = "#FDB827") + 
    # Change the y-axis to percent
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    # Change the x-axis breaks
    scale_x_continuous(breaks=seq(40, 120, 5)) +
    # Add labels to graph
    labs(title = "How Many Wins Needed for the Playoffs?",
         subtitle = "2012-2019 MLB Seasons",
         x = "Wins",
         y = "Playoff Probability",
         caption = "Data Visualization: @piRatesanalysis") +
    theme_bw() +
    theme(text = element_text(size = 20))

## We now want to determine marginal value of a win
## This is the marginal effects
## How much does a one win increase change the playoff odds?

## Create new data frame called wins.df that is sequeneced 0-162
## We want the playoff odds for each number of wins in a season
wins.df <- data.frame(W = seq(0:162))

## Use the playoffs model to predict playoff probability per win number
wins.df$Playoffs_Odds <- predict(playoffs.glm,
                                 newdata = wins.df,
                                 type = "response")

## Generate the difference between wins
## I.e. calculate marginal effect of a win
wins.df <- wins.df %>%
  arrange(W) %>%
  # Calcualte the marginal effect (the difference and needs to be lagged one)
  mutate(Playoff_Diff = Playoffs_Odds - lag(Playoffs_Odds))

## Plot the win curve
## Marginal effects of a win
## Calculate a smoothed line
spline <- as.data.frame(spline(wins.df$W, wins.df$Playoff_Diff))
wins.df %>%
  ggplot(aes(x = W, y = Playoff_Diff)) + 
    # Add in a line that is gold to smooth the points and create the win curve
    geom_line(data = spline, aes(x = x, y = y), color = "black", size = 2, alpha = 0.75) +
    # Change the y-axis to percent
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    # Add labels
    labs(title = "Win Curve: Playoff Probabilities",
         subtitle = "2012-2019 MLB Seasons",
         x = "Wins",
         y = "Marginal Effect",
         caption = "Data Visualization: @piRatesanalysis") +
    theme_bw() +
    theme(text = element_text(size = 20))

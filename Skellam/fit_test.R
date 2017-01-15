library(plyr)
# Linear map of points to a score between 0 and 1
map_to_score <- function(x) { 
  x_max <- max(x);
  return(x/x_max);
}
url_csv <- paste("http://www.football-data.co.uk/mmz4281/1516/E0.csv", 
                 sep="") # Data downloaded from football-data.co.uk
mydat   <- read.csv(url(url_csv)); epl <- c();
# teams are assigned IDs 1, 2, ...:
epl$home_team       <- as.numeric(mydat$HomeTeam)  
epl$away_team       <- as.numeric(mydat$AwayTeam)
epl$team_names      <- levels(mydat$HomeTeam)
epl$home_goals      <- mydat$FTHG # FTHG: full time home goals
epl$away_goals      <- mydat$FTAG # FTHG: full time away goals
epl$score_diff      <- epl$home_goals - epl$away_goals
# Points from last season are read and mapped to a score
epl$prev_perf       <- read.csv('DATA/prev_perf.csv', header = FALSE)
epl$prev_perf       <- map_to_score(epl$prev_perf[,2]) 
epl$nteams          <- length(unique(epl$home_team))
epl$ngames          <- length(epl$score_diff)
epl$nweeks          <- floor(2*epl$ngames/epl$nteams)
# The following code computes the week for each team in their games:
epl$home_week <- c();   epl$away_week <- c();
for (g in 1:epl$ngames) {
  epl$home_week[g]  <-  sum(epl$home_team[1:g] == epl$home_team[g]) + 
    sum(epl$away_team[1:g] == epl$home_team[g]) 
  epl$away_week[g]  <-  sum(epl$away_team[1:g] == epl$away_team[g]) +
    sum(epl$home_team[1:g] == epl$away_team[g])
}
epl$bet_home <- mydat$B365H; # Betting odds for home team win
epl$bet_draw <- mydat$B365D; # Betting odds for draw
epl$bet_away <- mydat$B365A; # Betting odds for away team win
saveRDS(epl,'epl_data.rds')
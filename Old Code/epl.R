# Reading and Munging the Data
library(plyr)
# Linear map of points to a score between -1 and 1
map_to_score <- function(x) { 
  x_max <- max(x);   x_min <- min(x);
  return(2*x/(x_max-x_min) - (x_max+x_min)/(x_max-x_min))
}
url_csv <- paste("http://www.football-data.co.uk/mmz4281/1516/E0.csv", 
                 sep="") # Data downloaded from football-data.co.uk
mydat   <- read.csv(url(url_csv))
# extracting relavant columns from 'mydat': 
epl <- c()
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
epl$home_week       <- c();
epl$away_week       <- c();
for (g in 1:epl$ngames) {
  epl$home_week[g]  <-  sum(epl$home_team[1:g] == epl$home_team[g]) + 
    sum(epl$home_team[1:g] == epl$away_team[g]) 
  epl$away_week[g]  <-  sum(epl$away_team[1:g] == epl$away_team[g]) +
    sum(epl$away_team[1:g] == epl$home_team[g])
}


# Fitting the Stan Model
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm <- stan_model("epl_model.stan")
for (w in 1:38) {
  print(w)
  epl_w <- epl
  idx <- (epl$home_week<=w) | (epl$away_week<=w)
  epl_w$home_team  <- epl$home_team[idx]
  epl_w$away_team  <- epl$away_team[idx]
  epl_w$home_goals <- epl$home_goals[idx]
  epl_w$away_goals <- epl$away_goals[idx]
  epl_w$score_diff <- epl$score_diff[idx]
  epl_w$home_week  <- epl$home_week[idx]
  epl_w$away_week  <- epl$away_week[idx]
  epl_w$ngames     <- length(epl_w$home_goals)
  epl_w$nweeks     <- max(c(epl_w$home_week, epl_w$away_week))
  fit <- sampling(sm, chains = 4, iter = 800, data = epl_w)
  saveRDS(fit, paste("FITS/fit_", w, ".rds", sep=""))
}



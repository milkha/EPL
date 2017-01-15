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


library("rstan")
epl <- readRDS("epl_data.rds")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm <- stan_model("epl_model_skellam.stan")
fit <- sampling(sm, chains = 4, iter = 500, data = epl)
f<-extract(fit)

# Coef plots
library (arm)
library(rstan)
library(matrixStats)
curr_perf <- read.csv('DATA/curr_perf.csv', header = FALSE)
sort_perf <- curr_perf[with(curr_perf, order(curr_perf[,2])), ]
a_hat <- colMeans(f$a[,38,])
a_se <- sqrt(colVars(f$a[,38,]))
coefplot (a_hat[order(curr_perf[,2])], a_se[order(curr_perf[,2])], 
          CI=1, varnames=as.character(sort_perf[,1]),
          main="Team abilities after week 38 (estimate +/- 1 s.e.)\n Teams are sorted according to total achieved points\n", 
          cex.var=.9, mar=c(1,6,5.1,2), xlim=c(-2,2))


# Evol plots
library (arm)
library(rstan)
library(matrixStats)
a_hat <- matrix(NA, nrow=38, ncol=20)
a_se <- matrix(NA, nrow=38, ncol=20)
for (w in 1:38) { 
  a_hat[w,] <- colMeans(f$a[,w,])
  a_se[w,] <- sqrt(colVars(f$a[,w,]))
}
a_min <- a_hat-a_se 
a_max <- a_hat+a_se
prev_perf <- read.csv('DATA/prev_perf.csv', header = FALSE)
sort_perf <- prev_perf[with(prev_perf, rev(order(prev_perf[,2]))), ]
png ("EPL_ability.png", height=10, width=8, units = 'in', res = 600)
attach(mtcars)
op <- par(mfrow = c(5,4),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0.8,0.8,4,4) + 0.1)
x<-c(1:38)
for (i in 1:20) {
  teamname <- sort_perf[i,1];  
  ind <- match(sort_perf[i,1], epl$team_names)
  plot(a_hat[,ind], type="l", ylim=c(-2,2),
       lty = 1, lwd = 3, bty='l')
  polygon(c(x, rev(x)), c(a_min[,ind], rev(a_max[,ind])), col = 'grey80', border = NA)
  lines(a_hat[,ind], type="l", ylim=c(-2,2),
        lty = 1, lwd = 3, bty='l')
  title(teamname, line=0)
  
  par(new = T)
  g1 <- lapply(ind, function(x) which(epl$home_team %in% x))
  g2 <- lapply(ind, function(x) which(epl$away_team %in% x))
  g <- c(g1[[1]],g2[[1]])
  scd <- epl$score_diff[g] * rep(c(1,-1), each=19)
  aa <- a_hat[,ind]
  scd <- scd[order(g)]
  plot(scd, col = 2, pch=16, axes=F, xlab=NA, ylab=NA, cex=0.7, ylim=c(-6,6))
  axis(side = 4, col="red",col.axis="red",las=1)
}
title(xlab = "week",
      ylab = "team ability",
      outer = TRUE, line = 3, cex.lab=1.5)
mtext("score difference",side=4,col="red",line=-1.5, outer = TRUE) 
par(op)
invisible(dev.off())

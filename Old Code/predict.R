library(rstan)
library(matrixStats)

epl<-readRDS('epl_data.rds')
a_sims <- array(NA, c(1600,20))
a_pred <- array(NA, c(38, 1600, 20))
b_home <- array(NA, c(38,1600))
nu <- array(NA, c(38,1600))
sigma_y <- array(NA, c(38,1600))
a_pred[1,,] <- matrix(rep(epl$prev_perf,1600), byrow=TRUE, nrow=1600)
b_home[1,] <- 0
nu[1,] <- 10
sigma_y[1,] <- 1

for (w in 1:37) { 
  fit <- readRDS(paste("FITS/fit_", w, ".RDS", sep=""))
  sims <- extract(fit)
  a_sims <- sims$a[,w,]
  a_pred[w+1,,] <- a_sims
  b_home[w+1,] <- sims$b_home
  nu[w+1,] <- sims$nu
  sigma_y[w+1,] <- sims$sigma_y
}

score_diff_pred <- array(NA, c(380,1600))
set.seed(1);
rt_ls <- function(n, df, mu, a) rt(n,df)*a + mu
for (i in 1:380) {
  w <- min(c(epl$home_week[i], epl$away_week[i]))
    for (j in 1:1600) {
      score_diff_pred[i,j] <- rt_ls(1, nu[w,j], 
                                  a_pred[epl$home_week[i], j, epl$home_team[i]] - 
                                  a_pred[epl$away_week[i], j, epl$away_team[i]],
                                  sigma_y[w,j]);
  }
}

library(matrixStats)
scd <- epl$score_diff[191:380]
scd_sims <- t(score_diff_pred[191:380,])
scd_hat <- colMeans(scd_sims, na.rm=TRUE)
scd_se <- sqrt(colVars(scd_sims, na.rm=TRUE))
scd_ub <- scd_hat + 1.95 * scd_se;
scd_lb <- scd_hat - 1.95 * scd_se;
scd_ub2 <- scd_hat + 0.67 * scd_se;
scd_lb2 <- scd_hat - 0.67 * scd_se;

sort_scd <- scd[order(scd)]
sort_scd_hat <- scd_hat[order(scd)]
sort_scd_se <- scd_se[order(scd)]
sort_scd_ub <- scd_ub[order(scd)]
sort_scd_lb <- scd_lb[order(scd)]
sort_scd_ub2 <- scd_ub2[order(scd)]
sort_scd_lb2 <- scd_lb2[order(scd)]
df <- data.frame(list(scd = sort_scd, scd_hat = sort_scd_hat, scd_se = sort_scd_se, 
                      scd_ub = sort_scd_ub, scd_lb = sort_scd_lb, 
                      scd_ub2 = sort_scd_ub2, scd_lb2 = sort_scd_lb2))

ggplot(df, aes(x = c(1:190))) +
  geom_ribbon(aes(ymin = scd_lb,
                  ymax = scd_ub),
              fill="lightyellow") + 
  geom_ribbon(aes(ymin = scd_lb2,
                  ymax = scd_ub2),
              fill="khaki3") + 
  geom_line(aes(y=scd_hat),colour="darkred") + 
  geom_point(aes(y=scd), size = 0.3) +
  scale_x_continuous(name="match") +
  scale_y_continuous(name="score difference", minor_breaks = seq(-6, 6, 1)) +
  ggtitle("Estimated score differences (red) with 95% intervals (light yellow), \n  50% intervals (dark yellow), and the actual score differences (black)");




library(matrixStats)
scd <- epl$score_diff[191:380]
scd_sims <- t(score_diff_pred[191:380,])
scd_hat <- colMedians(scd_sims, na.rm = TRUE)
alpha <- 0.95
scd_ub <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2, na.rm = TRUE)
scd_lb <- colQuantiles(scd_sims, probs = (1-alpha)/2, na.rm = TRUE)
ci95 <- sum(scd < scd_ub & scd_lb<scd)/190
alpha <- 0.5
scd_ub <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2, na.rm = TRUE)
scd_lb <- colQuantiles(scd_sims, probs = (1-alpha)/2, na.rm = TRUE)
ci50 <- sum(scd < scd_ub & scd_lb<scd)/190


summ<-0
sum_vec<-array(0,190)
scd_h <-round(scd_hat);
for (i in 1:190) {
  if (scd[i]>0 & scd_h[i]>0)
    summ<-summ+epl$bet_home[i+190];
  if (scd[i]<0 & scd_h[i]<0)
    summ<-summ+epl$bet_away[i+190];
  if (scd[i]==0 & scd_h[i]==0)
    summ<-summ+epl$bet_draw[i+190];
  summ<-summ-1;
  sum_vec[i] <- summ;
}
plot(sum_vec,type="l")




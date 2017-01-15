library(rstan)
library(matrixStats)
nsamples=1500
epl <- readRDS('epl_data.rds')
a_sims<-readRDS("FITS/a_sims.rds")
b_home <- array(NA, c(38,nsamples))
nu <- array(NA, c(38,nsamples))
sigma_y <- array(NA, c(38,nsamples))
for (w in 1:38) { 
  fit <- readRDS(paste("FITS/fit_", w, ".RDS", sep=""))
  sims <- extract(fit)
  b_home[w,] <- sims$b_home
  nu[w,] <- sims$nu
  sigma_y[w,] <- sims$sigma_y
}
score_diff_pred <- array(NA, c(380,nsamples))
set.seed(1);

rt_ls <- function(n, df, mu, a) rt(n,df)*a + mu
for (i in 11:380) {
  w <- ceiling(i/10)
  for (j in 1:nsamples) {
    score_diff_pred[i,j] <- 
      rt_ls(1, nu[w-1,j], 
            a_sims[j,epl$home_week[i]-1, epl$home_team[i]] - 
              a_sims[j,epl$away_week[i]-1, epl$away_team[i]] +
              b_home[w-1,j],
            sigma_y[w-1,j]);
  }
}

library(matrixStats)
scd <- epl$score_diff[191:380]
scd_sims <- round(t(score_diff_pred[191:380,]))

pred_probs <- matrix(0,3,190)
pred_probs[1,] <- colSums(scd_sims>0)/nsamples
pred_probs[2,] <- colSums(scd_sims==0)/nsamples
pred_probs[3,] <- colSums(scd_sims<0)/nsamples



sum_vec<-array(0,c(1000,190))
for (j in 1:10000) {
  summ<-0
for (i in 1:190) {
  xx <- sample(x=c(1,0,-1), prob=pred_probs[,1], size=1)
  if (scd[i]>0 & xx>0)
    summ<-summ+epl$bet_home[i+190];
  if (scd[i]<0 & xx<0)
    summ<-summ+epl$bet_away[i+190];
  if (scd[i]==0 & xx==0)
    summ<-summ+epl$bet_draw[i+190];
  summ<-summ-1
  sum_vec[j,i] <- summ;
}
}


scd_hat <- colMedians(sum_vec, na.rm=TRUE)
scd_se <- sqrt(colVars(sum_vec, na.rm=TRUE))
scd_ub <- scd_hat + 1.95 * scd_se;
scd_lb <- scd_hat - 1.95 * scd_se;
scd_ub2 <- scd_hat + 0.67 * scd_se;
scd_lb2 <- scd_hat - 0.67 * scd_se;

df <- data.frame(list(scd = scd, scd_hat = scd_hat, scd_se = scd_se, 
                      scd_ub = scd_ub, scd_lb = scd_lb, 
                      scd_ub2 = scd_ub2, scd_lb2 = scd_lb2))

ggplot(df, aes(x = c(1:190))) +
  geom_ribbon(aes(ymin = scd_lb,
                  ymax = scd_ub),
              fill="lightyellow") + 
  geom_ribbon(aes(ymin = scd_lb2,
                  ymax = scd_ub2),
              fill="khaki3") + 
  geom_line(aes(y=scd_hat),colour="darkred") + 
  scale_x_continuous(name="match") +
  scale_y_continuous(name="score difference", minor_breaks = seq(-6, 6, 1)) +
  ggtitle("Predicted score differences (red) with 95% intervals (light yellow), \n  50% intervals (dark yellow), and the actual score differences (black)");
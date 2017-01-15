functions {
  real skellam_lpmf(int y, real mu_1, real mu_2) {
    return modified_bessel_first_kind( abs(y), 2*sqrt(mu_1*mu_2) ) * 
           ((mu_1/mu_2)^(y*0.5)) * 
           exp(- mu_1 - mu_2);
  }
}

data {
  int<lower=1> nteams; // number of teams (20)
  int<lower=1> ngames; // number of games 
  int<lower=1> nweeks; // number of weeks 
  int<lower=1> home_week[ngames]; // week number for the home team
  int<lower=1> away_week[ngames]; // week number for the away team
  int<lower=1, upper=nteams> home_team[ngames]; // home team ID (1, ..., 20)
  int<lower=1, upper=nteams> away_team[ngames]; // away team ID (1, ..., 20)
  int score_diff[ngames];    // home_goals - away_goals
  row_vector<lower=0>[nteams] prev_perf; // a score between 0 and +1
}
parameters {
  real b_home; // the effect of hosting the game in mean of score_diff dist.
  matrix<lower=0>[nweeks,nteams] eta_a;         // random component
}
transformed parameters {
  matrix<lower=0>[nweeks, nteams] a;                        // team abilities
  a[1] =  prev_perf .* eta_a[1]; // initial abilities (at week 1)
  for (w in 2:nweeks) {
    a[w] = a[w-1] .* eta_a[w];           // evolution of abilities
  }
}
model {
  // Priors
  b_home ~ normal(0,1);
  to_vector(eta_a) ~ normal(1,0.05);
  // Likelihood
  for (g in 1:ngames) {
    score_diff[g] ~ skellam(a[home_week[g],home_team[g]] + b_home, a[away_week[g],away_team[g]]);
  }
}




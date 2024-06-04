data {
  int N_obs;
  int N_countries;
  
  vector[N_obs] pop;
  array[N_obs] int cases;
  array[N_obs] int country;
}

parameters {
  vector[N_countries] log_theta;
  real mu_theta;
  real<lower = 0> sigma_theta;
  real<lower = 0> phi_inv;
}

transformed parameters {
  vector[N_countries] theta = exp(log_theta);
  real<lower = 0> phi = 1 / phi_inv;
}

model {
  cases ~ neg_binomial_2(pop .* theta[country], phi);
  log_theta ~ normal(mu_theta, sigma_theta);
  phi_inv ~ exponential(1);
}



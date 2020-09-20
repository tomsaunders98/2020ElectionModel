data {
  //Totals
  int N; // number of state polls
  int S; // number of states
  int D; // "days"
  int last_poll_D;
  //Indexes
  int<lower = 1, upper = D> day[N];
  int<lower = 1, upper = S+1> state[N];
  // Polls
  int n_dem[N];
  int <lower = 0> n_respondents[N];
  //Priors
  vector[S] mu_state_prior;
  matrix [S, S] sigma_mu_state_end;
  matrix [S, S] sigma_walk_state_forecast; 
}

transformed data {
  //Cholesky Factors
  cholesky_factor_cov[S]  chol_sigma_mu_state_end;
  cholesky_factor_cov[S]  chol_sigma_walk_state_forecast;
  //Cholesky decompositions
  chol_sigma_walk_state_forecast = cholesky_decompose(sigma_walk_state_forecast);
  chol_sigma_mu_state_end = cholesky_decompose(sigma_mu_state_end);
  
}

parameters {
  real<lower = 0, upper = 0.05> sigma_walk_natl_past; //uniform priors for sigma vals
  matrix[S, D] delta_state;
  vector[last_poll_D] delta_natl;
  vector[S] mu_state_end;
  
}

transformed parameters {
  //Initilaise Vectors
  real logit_dem_pi[N];
  vector[last_poll_D] mu_natl;
  matrix[S, D] mu_state;
  //mu_national + mu_state priors
  mu_state[:, D] =  chol_sigma_mu_state_end * mu_state_end + mu_state_prior;
  mu_natl[last_poll_D] = 0;
  //mu_national reverse random walk
  for (i in 1:last_poll_D-1){
    mu_natl[last_poll_D-i] = mu_natl[last_poll_D-i+1] + sigma_walk_natl_past*delta_natl[last_poll_D-i];
  }
  //mu_state reverse random walk
  for (i in 1:(D-1)) {
    mu_state[:, D-i] = mu_state[:, D-i+1] + chol_sigma_walk_state_forecast*delta_state[:, D - i];
  }
  //adding components for latent voter intention
  for(i in 1:N){
    logit_dem_pi[i] = mu_natl[day[i]] + mu_state[state[i], day[i]];
  }
  
}

model {
  //priors
  delta_natl ~ std_normal();
  mu_state_end ~ std_normal();
  to_vector(delta_state) ~ std_normal();
  //Liklihood
  n_dem ~ binomial_logit(n_respondents, logit_dem_pi);
}

generated quantities {
  //generate predicted score
  matrix[D, S] predicted_score;
  for(s in 1:S){ 
    for (d in 1:D){
      if (d < last_poll_D+1){
        predicted_score[1:last_poll_D, s] = inv_logit( mu_natl[1:last_poll_D] + to_vector(mu_state[s, 1:last_poll_D]));
      }else{
        predicted_score[last_poll_D:D, s] = inv_logit(to_vector(mu_state[s, last_poll_D:D]));
      }
    }
  }
}

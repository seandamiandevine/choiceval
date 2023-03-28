
// see https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html

data {
  int<lower=0> N;
  int<lower=0> T;
  int<lower=1, upper=T> Tsubj[N]; 
  int  mutual_info[N,T];
  int  choose_choice[N,T];
  int  chosen_deck[N,T];
  int  win_card[N,T];
  int  outcome[N,T];
  int  is_mock[N,T];
  real mock_rating[N,T];
  
}

parameters {
	// group means
	// parameters : 
	  // mu[1] = learning rate
	  // mu[2] = inv. temp.
	vector[2] mu; 
	
	// random variances
	vector<lower=0>[2] tau;	
	
	// reparameterization stuff
	matrix[2, N] U_pr;            // uncorreleated random effects
  cholesky_factor_corr[2] L_u;  // decomposition of corr matrix to lower triangular matrix
}


transformed parameters {
	matrix[N, 2] U;
  real alpha[N];
  real beta[N];

  // transform random effects
  U = (diag_pre_multiply(tau, L_u) * U_pr)';
  
  // save named pars.
  for(i in 1:N) {
    alpha[i] = Phi_approx(mu[1] + U[i,1]);    // learning rate
    beta[i]  = exp(mu[2] + U[i,2]);           // inv. temp
  }
}


model {
  // priors (adjusted for bounds)
 target += normal_lpdf(mu | 0, 1);
 target += normal_lpdf(tau | 0, 10) - 2*normal_lccdf(0 | 0, 10);
 target += lkj_corr_cholesky_lpdf(L_u | 2);
 target += std_normal_lpdf(to_vector(U_pr));
  
  // likelihood
  for(i in 1:N) {
    vector[2] Q; 
    Q = rep_vector(0, 2); // init Q at 0 [A, B]
    
    for(t in 1:Tsubj[i]) {

      // Likelihood
      target += bernoulli_logit_lpmf(choose_choice[i,t] | beta[i] * (Q[1] - Q[2]) );
      
      // update Q
      Q[choose_choice[i,t]+1] = Q[choose_choice[i,t]+1] + alpha[i]*(outcome[i,t] - Q[choose_choice[i,t]+1]);
    }   
  }
}

generated quantities {
  // "recompose" Choleksy to corr matrix
  corr_matrix[2] rho = L_u * L_u';
  
  // named pars.
  real mu_alpha = Phi_approx(mu[1]);
  real mu_beta  = exp(mu[2]);
  
  // save log lik for model comparison
  matrix[N,T] log_lik;
  for(i in 1:N) {
    for (t in 1:T) {
      log_lik[i,t] = 0; // init at 0 to prevent NAs
    }
  }

  for(i in 1:N) {
    vector[2] Q; 
    Q = rep_vector(0, 2); // init Q at 0 [A, B]
    
    for(t in 1:Tsubj[i]) {

      // Likelihood
      log_lik[i,t] = bernoulli_logit_lpmf(choose_choice[i,t] | beta[i] * (Q[1] - Q[2])); 

      // update Q
      Q[choose_choice[i,t]+1] = Q[choose_choice[i,t]+1] + alpha[i]*(outcome[i,t] - Q[choose_choice[i,t]+1]);
    }
  }
  
  
}


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
  int  mi_pe[N,T];
  int  is_mock[N,T];
  real mock_rating[N,T];
  int  trialinblock[N,T];
  
}

parameters {
	// group means
	// parameters : 
	  // mu[1] = learning rate
	  // mu[2] = inv. temp.
    // mu[3] = value of choice
    // mu[4] = MI:VoC
	vector[4] mu; 
	
	// random variances
	vector<lower=0>[4] tau;	
	
	// reparameterization stuff
	matrix[4, N] U_pr;            // uncorreleated random effects
  cholesky_factor_corr[4] L_u;  // decomposition of corr matrix to lower triangular matrix
}


transformed parameters {
	matrix[N, 4] U;
  real alpha[N];
  real beta[N];
  real phi_c[N];
  real phi_cm[N];
  
  // transform random effects
  U = (diag_pre_multiply(tau, L_u) * U_pr)';
  
  // save named pars.
  for(i in 1:N) {
    alpha[i]  = Phi_approx(mu[1] + U[i,1]);    // learning rate
    beta[i]   = exp(mu[2] + U[i,2]);           // inv. temp
    phi_c[i]  = mu[3] + U[i,3];                // value of choice
    phi_cm[i] = mu[4] + U[i,4];                // interaction MI:Voc
  }
}


model {
  
  // priors (adjusted for bounds)
 target += normal_lpdf(mu | 0, 1);
 target += normal_lpdf(tau | 0, 10) - 4*normal_lccdf(0 | 0, 10);
 target += lkj_corr_cholesky_lpdf(L_u | 2);
 target += std_normal_lpdf(to_vector(U_pr));
  
  // likelihood
  for(i in 1:N) {
    // priors for beta-binomial learning
    real a;
    real b;
    real pT;

    // init Q values
    vector[2] Q; 
    Q = rep_vector(0, 2); // init Q at 0 [A, B]
    Q[1] += phi_c[i];     // add VoC
    
    a  = 1;
    b  = 1;
    pT = a/(a+b);
    for(t in 1:Tsubj[i]) {
      
      if(trialinblock[i,t]==0) {
        // reset learning on new block
        a = 1;
        b = 1;
      }
      
      // Likelihood
      // Increase VoC in choice rule, depending on pT
      // (pT main effects in Q_A vs. Q_B cancel out)
      target += bernoulli_logit_lpmf(choose_choice[i,t] | beta[i] * ( (Q[1] + phi_c[i] + phi_cm[i]*pT ) - Q[2]) );
      
      // update Q
      Q[choose_choice[i,t]+1] = Q[choose_choice[i,t]+1] + alpha[i]*(outcome[i,t] - Q[choose_choice[i,t]+1]);
      
      // update pT
      a += mi_pe[i,t];
      b += 1-mi_pe[i,t];
      pT = a/(a+b);
    } // end of trial loop
  } // end of subject loop
} // end of model

generated quantities {
  // "recompose" Choleksy to corr matrix
  corr_matrix[4] rho = L_u * L_u';
  
  // named pars.
  real mu_alpha  = Phi_approx(mu[1]);
  real mu_beta   = exp(mu[2]);
  real mu_phi_c  = mu[3];
  real mu_phi_mc = mu[4];

  // save log lik for model comparison
  matrix[N,T] log_lik;
  for(i in 1:N) {
    for (t in 1:T) {
      log_lik[i,t] = 0; // init at 0 to prevent NAs
    }
  };
  
    for(i in 1:N) {
    // init Q values
    real a;
    real b;
    real pT;

    // priors for beta-binomial learning
    vector[2] Q; 
    Q = rep_vector(0, 2); // init Q at 0 [A, B]
    Q[1] += phi_c[i];     // add VoC
    
    a  = 1;
    b  = 1;
    pT = a/(a+b);
    for(t in 1:Tsubj[i]) {
      
      if(trialinblock[i,t]==0) {
        // reset learning on new block
        a = 1;
        b = 1;
      }
      
      // Likelihood
      // Increase VoC in choice rule, depending on pT
      // (pT main effects in Q_A vs. Q_B cancel out)
      log_lik[i,t] += bernoulli_logit_lpmf(choose_choice[i,t] | beta[i] * ( (Q[1] + phi_c[i] + phi_cm[i]*pT )  - Q[2]));
      
      // update Q
      Q[choose_choice[i,t]+1] = Q[choose_choice[i,t]+1] + alpha[i]*(outcome[i,t] - Q[choose_choice[i,t]+1]);
      
      // update pT
      a += mi_pe[i,t];
      b += 1-mi_pe[i,t];
      pT = a/(a+b);
    } // end of trial loop
  } // end of subject loop
} // end of generated quantities

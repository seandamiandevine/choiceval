
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
	  // mu[3] = VoC
	  // mu[4] = MI:VoC
	  // mu[5] = lr MI
	vector[5] mu; 
	
	// random variances
	vector<lower=0>[5] tau;	
	
	// Subject-level raw parameters (for Matt trick)
  vector[N] alpha_pr;
  vector[N] beta_pr;
  vector[N] phi_c_pr;
  vector[N] phi_m_pr;
  vector[N] eta_pr;
}

transformed parameters {
// Transform subject-level raw parameters
  vector<lower=0,upper=1>[N] alpha;
  vector<lower=0>[N] beta; 
  vector[N] phi_c; 
  vector[N] phi_m; 
  vector<lower=0,upper=1>[N] eta; 

  for (i in 1:N) {
      alpha[i] = Phi_approx( mu[1] + tau[1] * alpha_pr[i]); 
      beta[i]  = exp(        mu[2] + tau[2] * beta_pr[i] );
      phi_c[i] =             mu[3] + tau[3] * phi_c_pr[i];
      phi_m[i] =             mu[4] + tau[4] * phi_m_pr[i];
      eta[i]   = Phi_approx( mu[5] + tau[5] * eta_pr[i]  );
  }
}



model {
  
  // priors (adjusted for bounds)
 target += normal_lpdf(mu  | 0, 1);
 target += normal_lpdf(tau | 0, 10) - 5*normal_lccdf(0 | 0, 10);
 
 // untransformed pars for reparameterization
 target += normal_lpdf(alpha_pr | 0,1);
 target += normal_lpdf(beta_pr  | 0,1);
 target += normal_lpdf(phi_c_pr | 0,1);
 target += normal_lpdf(phi_m_pr | 0,1);
 target += normal_lpdf(eta_pr   | 0,1);

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
      target += bernoulli_logit_lpmf(choose_choice[i,t] | beta[i] * ( (Q[1] + phi_c[i] + phi_m[i]*pT ) - Q[2]) );
      
      // update Q
      Q[choose_choice[i,t]+1] = Q[choose_choice[i,t]+1] + alpha[i]*(outcome[i,t] - Q[choose_choice[i,t]+1]);
      
      // update pT
      a += eta[i]*(mi_pe[i,t]);
      b += eta[i]*(1-mi_pe[i,t]);
      pT = a/(a+b);
    } // end of trial loop
  } // end of subject loop
} // end of model

generated quantities {
  
  // named pars.
  real mu_alpha  = Phi_approx(mu[1]);
  real mu_beta   = exp(mu[2]);
  real mu_phi_c  = mu[3];
  real mu_phi_m  = mu[4];
  real mu_eta    = Phi_approx(mu[5]);


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
      log_lik[i,t] += bernoulli_logit_lpmf(choose_choice[i,t] | beta[i] * ( (Q[1] + phi_c[i] + phi_m[i]*pT )  - Q[2]));
      
      // update Q
      Q[choose_choice[i,t]+1] = Q[choose_choice[i,t]+1] + alpha[i]*(outcome[i,t] - Q[choose_choice[i,t]+1]);
      
      // update pT
      a += eta[i]*(mi_pe[i,t]);
      b += eta[i]*(1-mi_pe[i,t]);
      pT = a/(a+b);
    } // end of trial loop
  } // end of subject loop
} // end of generated quantities

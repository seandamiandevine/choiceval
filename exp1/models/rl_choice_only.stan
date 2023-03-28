
// see https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html

data {
  int<lower=0> N;
  int<lower=0> J[N];
  int C_L[N,max(J)];
  int C_R[N,max(J)];
  int M_L[N,max(J)];
  int M_R[N,max(J)];
  int deck_l[N,max(J)];
  int deck_r[N,max(J)];
  int choose_L[N,max(J)];
  int chosen_deck[N,max(J)];
  real reward[N,max(J)];
}

parameters {
  real G_A;                // Unconstrained fixed effect for lr 
  real <lower=0> TAU_A;    // Random variance for lr 
  vector[N] b_a_raw;       // Unconstrained random effects for lr used for Matt trick 
  
  real <lower=0> BETA;     // Fixed effect for temperature
  real <lower=0> TAU_BETA; // Random variance for temperature
  vector[N] beta_raw;      // Random effects for temperature used Matt trick 

  real PHI_C;               // Fixed effect for VoC
  real <lower=0> TAU_PHI_C; // Random variance for VoC
  vector[N] phi_c_raw;      // Random effects for VoC used for Matt trick 

  real  PHI_M;              // Fixed effect for VoMI
  real <lower=0> TAU_PHI_M; // Random variance for VoMI
  vector[N] phi_m_raw;      // Random effects for VoMI used for Matt trick 

}

transformed parameters {
  vector[N] b_a;
  vector[N] beta; 
  vector[N] phi_c; 
  vector[N] phi_m;

  // Implement Matt trick: 
  // implies: b_a ~ normal(G_A, TAU_A)
  b_a = G_A + TAU_A * b_a_raw;
  
  // implies: beta ~ normal(BETA, TAU_BETA)
  beta = BETA + TAU_BETA * beta_raw;
  
  // implies: phi_c ~ normal(PHI_C, TAU_PHI_C)
  phi_c = PHI_C + TAU_PHI_C * phi_c_raw;
  
  // implies: phi_m ~ normal(PHI_M, TAU_PHI_M)
  phi_m = PHI_M + TAU_PHI_M * phi_m_raw;

}

model {
  real pL;        // P(Choose left)
  real QL;        // value of L 
  real QR;        // value of R
  real lr;        // temporary learning rate
  vector[4] Q;    // Q-values to update

  // prior on fixed effects
  G_A       ~ normal(0,1);
  TAU_A     ~ gamma(2,2);
  
  BETA      ~ gamma(4.83, 1/0.73); // https://gershmanlab.com/pubs/Gershman16.pdf
  TAU_BETA  ~ gamma(2,2);
  
  PHI_C     ~ normal(0,1);
  TAU_PHI_C ~ gamma(2,2);
  
  PHI_M     ~ normal(0,1);
  TAU_PHI_M ~ gamma(2,2);  
  
  // priors on random effects (using Matt trick)
  b_a_raw   ~ std_normal(); 
  beta_raw  ~ std_normal(); 
  phi_c_raw ~ std_normal(); 
  phi_m_raw ~ std_normal(); 
  
  // likelihood
  for(i in 1:N) {
    Q = rep_vector(0, 4); // init Q at 0.5
  
    for(j in 1:J[i]) {
      // compute Q-values, with added VoC and VoMI
      QL = Q[deck_l[i,j]] + phi_c[i]*C_L[i,j] + phi_m[i]*M_L[i,j];
      QR = Q[deck_r[i,j]] + phi_c[i]*C_R[i,j] + phi_m[i]*M_R[i,j];
      
      // P(Choose Left)
      pL = exp(beta[i]*QL)/(exp(beta[i]*QL) + exp(beta[i]*QR));
      
      // Likelihood
      choose_L[i,j] ~ bernoulli(pL);
      
      // update Q, with computed lr with VoC and VoMI
      // Q[chosen_deck[i,j]] = Q[chosen_deck[i,j]] + a[i]*(reward[i,j] - Q[chosen_deck[i,j]]);
      
      if(choose_L[i,j]==1) {
        lr = inv_logit(b_a[i]);
        Q[chosen_deck[i,j]] = QL + lr*(reward[i,j] - QL);
      } else {
        lr = inv_logit(b_a[i]);
        Q[chosen_deck[i,j]] = QR + lr*(reward[i,j] - QR);
      }
    }
  }
}

generated quantities {
  real <lower=0,upper=1> A;
  vector<lower=0,upper=1>[N] a;
  matrix[N,max(J)] log_lik; 

  // link functions
  A = inv_logit(G_A);
  a = inv_logit(b_a);
  
  // reproduce likelihood for waic calculation
  for(i in 1:N) {
    vector[4] Q; 
    Q = rep_vector(0, 4); // init Q at 0.5
  
    for(j in 1:J[i]) {
      // compute Q-values, with added VoC and VoMI
      real QL = Q[deck_l[i,j]] + phi_c[i]*C_L[i,j] + phi_m[i]*M_L[i,j];
      real QR = Q[deck_r[i,j]] + phi_c[i]*C_R[i,j] + phi_m[i]*M_R[i,j];
      
      // P(Choose Left)
      real pL = exp(beta[i]*QL)/(exp(beta[i]*QL) + exp(beta[i]*QR));
      
      // update Q, with computed lr with VoC and VoMI
      real lr = inv_logit(b_a[i]);
      if(choose_L[i,j]==1) {
        Q[chosen_deck[i,j]] = QL + lr*(reward[i,j] - QL);
      } else {
        Q[chosen_deck[i,j]] = QR + lr*(reward[i,j] - QR);
      }
      
      // save likelihood 
      log_lik[i,j] = bernoulli_lpmf(choose_L[i,j] | pL); 
    }
  }
}

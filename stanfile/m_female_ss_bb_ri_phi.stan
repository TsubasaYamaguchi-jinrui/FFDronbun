data {
  int len_obs;
  int N;
  int D; 
  matrix[len_obs, D] X; 
  array[len_obs] int no_female;
  array[len_obs] int max_female;
  array[len_obs] int obs_no;
}

parameters {
  vector[D] b;
  vector[D] gamma; 
  real<lower=0> s_t;
  vector[N] mu_err;  
  vector[len_obs] r_raw;  
  vector[len_obs] z_raw;  
  real<lower=0> s_w; 
  real<lower=0> s_z; 
}

transformed parameters {
  
  vector[N] mu;
  
  mu[1] = mu_err[1];
  
  for(i in 2:N){
    mu[i] = mu[i-1] + s_t*mu_err[i];   
  }

  vector[len_obs] p;
  
  for (i in 1:len_obs){
    p[i] = inv_logit(mu[obs_no[i]] + X[i]*b + s_w*r_raw[i]);
  }
  
  vector[len_obs] phi;  
  
  for (i in 1:len_obs){
    phi[i] = exp(X[i]*gamma + s_z*z_raw[i]);
  }
  
}


model {
  
  mu_err ~ normal(0,1);  
  r_raw ~ normal(0,1); 
  z_raw ~ normal(0,1); 
  
  for(i in 1:len_obs){
    no_female[i] ~ beta_binomial(max_female[i], p[i]*phi[i] + 1E-10, phi[i]*(1-p[i]) + 1E-10);
  }
  
  b ~ student_t(4,0,5);  
  gamma ~ student_t(4,0,5);  
  
  s_t ~ student_t(4,0,2.5);  
  s_w ~ student_t(4,0,2.5);  
  s_z ~ student_t(4,0,2.5); 
}

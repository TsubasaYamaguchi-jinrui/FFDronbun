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
  real<lower=0> s_t;   
  vector[N] mu_err;    
  real<lower=0> phi;   
  vector[len_obs] r_raw;  
  real<lower=0> s_w; 
}

transformed parameters {
  
  vector[N] mu;  
  
  mu[1] = mu_err[1];
  mu[2] = mu_err[2]; 
  
  for(i in 3:N){
  mu[i] = 2*mu[i-1] - mu[i-2] + mu_err[i]*s_t; 
  }

  vector[len_obs] p;  
  
  for (i in 1:len_obs){
    p[i] = inv_logit(mu[obs_no[i]] + X[i]*b + s_w*r_raw[i]);
  }
  
}


model {
  
  mu_err ~ normal(0,1); 
  r_raw ~ normal(0,1);
  
  for(i in 1:len_obs){
    no_female[i] ~ beta_binomial(max_female[i], p[i]*phi + 1.0E-10, phi*(1-p[i]) + 1.0E-10);
  }
  
  b ~ student_t(4,0,5);
  
  phi ~ gamma(0.01, 0.01); 
  
  s_t ~ student_t(4,0,2.5);
  s_w ~ student_t(4,0,2.5);  
  
}

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
  vector[2] mu_ini; 
  vector[D] b;   
  real<lower=0> s_t;   
  vector[N-2] mu_err;    
}

transformed parameters {
  
  vector[N] mu;  
  
  mu[1:2] = mu_ini[1:2];
  
  for(i in 3:N){
  mu[i] = 2*mu[i-1] - mu[i-2] + mu_err[i-2]*s_t; 
  }

  vector[len_obs] logit_p;  
  
  for (i in 1:len_obs){
    logit_p[i] = mu[obs_no[i]] + X[i]*b;
  }
  
}


model {
  
  mu_err ~ normal(0,1); 
  
  for(i in 1:len_obs){
    no_female[i] ~ binomial_logit(max_female[i], logit_p[i]);
  }
  
  b ~ student_t(4,0,5);
  
  s_t ~ student_t(4,0,2.5);
  
}

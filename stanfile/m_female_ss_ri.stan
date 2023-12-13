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
  real<lower=0> s_w;  
  vector[N] mu_err; 
  vector[N] r; 
}

transformed parameters {
  
  vector[N] mu;
  
  mu[1] = mu_err[1];
  
  for(i in 2:N){
    mu[i] = mu[i-1] + s_t*mu_err[i];
  }

  vector[N] p;
  
  for (i in 1:len_obs){
    p[obs_no[i]] = mu[obs_no[i]] + X[i]*b + r[obs_no[i]];
  }
  
}


model {
  
  r ~ normal(0, s_w); 
  
  mu_err ~ normal(0,1);
  
  for(i in 1:len_obs){
    no_female[i] ~ binomial_logit(max_female[i], p[obs_no[i]]);
  }
  
  b ~ student_t(3,0,5);
  
  s_t ~ student_t(3, 0, 2.5);
  
  s_w ~ student_t(3,0,2.5); 
  
}

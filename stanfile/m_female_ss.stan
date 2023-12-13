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
}

transformed parameters {
  
  vector[N] mu;
  
  mu[1] = mu_err[1];
  
  for(i in 2:N){
    mu[i] = mu[i-1] + s_t*mu_err[i];
  }

  vector[N] p;
  
  for (i in 1:len_obs){
    p[obs_no[i]] = mu[obs_no[i]] + X[i]*b;
  }
  
}


model {
  
  mu_err[2:N] ~ normal(0,1);
  
  for(i in 1:len_obs){
    no_female[i] ~ binomial_logit(max_female[i],p[obs_no[i]]);
  }
  
  b ~ student_t(4,0,10);
  
  s_t ~ student_t(4,0,5);
  
}

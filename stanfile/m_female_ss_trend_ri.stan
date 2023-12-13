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
  vector[N] mu;   
  vector[N] r;   
}

transformed parameters {
  
  vector[N] p;
  
  for (i in 1:len_obs){
    p[obs_no[i]] = mu[obs_no[i]] + X[i]*b + r[obs_no[i]];
  }
  
}


model {
  
  r ~ normal(0, s_w); 
  
  for(i in 3:N){
    mu[i] ~ normal(2*mu[i-1] - mu[i-2], s_t); 
  }
  
  for(i in 1:len_obs){
    no_female[i] ~ binomial_logit(max_female[i], p[obs_no[i]]);
  }
  
  b ~ student_t(4,0,5);    
   
  s_t ~ student_t(3,0,2.5);     
  s_w ~ student_t(3,0,2.5);   
  
}


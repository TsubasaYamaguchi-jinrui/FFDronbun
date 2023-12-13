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
  vector[N] mu;    
}

transformed parameters {

  vector<lower=0,upper=1>[len_obs] p;  
  
  for (i in 1:len_obs){
    p[i] = inv_logit(mu[obs_no[i]] + X[i]*b);
  }
  
}


model {

  mu[3:N] ~ normal(2*mu[2:(N-1)] - mu[1:(N-2)], s_t); 
  
  for(i in 1:len_obs){
    no_female[i] ~ binomial(max_female[i],p[i]);
  }
  
  b ~ student_t(4,0,5);
  
  s_t ~ student_t(4,0,5);
  
}

// Photosynthesis -- irradiance curve model
// Not finished

functions {
  vector saturating (real pmax, real alpha, real rd, vector x) {
    return pmax * (1-exp(-alpha/pmax * x)) - rd;
  }
  real saturating_real (real pmax, real alpha, real rd, real x) {
    return pmax * (1-exp(-alpha/pmax * x)) - rd;
  }
  vector inhibiting (real pmax, real alpha, real rd, vector x) {
    return pmax * (1-exp(-alpha/pmax * x)) - rd;
  }
  real inhibiting_real (real pmax, real alpha, real rd, real x) {
    return pmax * (1-exp(-alpha/pmax * x)) - rd;
  }
}
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N; // Number of observations
  int<lower=1> M; // Number of predictions
  int<lower=1, upper=2> K; // Model selector
  vector<lower=0>[N] ppfd;
  vector[N] pnet;
  real priormuPMAXmu;
  real priormuALPHAmu;
  real priorBETAmu;
  real priormuRESPmu;
  real priormuPMAXsigma;
  real priormuALPHAsigma;
  real priormuBETAsigma;
  real priormuRESPsigma;
  real<lower=0> priorCauchy;
  real max_ppfd;

}
transformed data {
  vector[M] light;
  light[1] = 0.0;
  for(m in 2:M) {
    light[m] = light[m-1] + max_ppfd / M;
  }
}
parameters {
  real<lower = 0> PMAX;
  real<lower = 0> ALPHA;
  //real<lower = 0> BETA;
  real RESP;
  real<lower = 0> sigmaNP;
}
transformed parameters {
  vector[N] pnet_fit;
  for(n in 1:N) {
    pnet_fit[n] = saturating_real(PMAX,ALPHA,RESP, ppfd[n]);
  }
}

model {
  int pos = 1;
  // Prior distribution
  muPMAX  ~ normal(priormuPMAXmu,   priormuPMAXsigma);
  muALPHA ~ lognormal(priormuALPHAmu,  priormuALPHAsigma);
  muRESP  ~ normal(priormuRESPmu,   priormuRESPsigma);

  PMAX  ~ normal(muPMAX, sigmaPMAX);
  ALPHA ~ lognormal(log(muALPHA), sigmaALPHA);
  RESP  ~ normal(muRESP, sigmaRESP);
  sigmaPMAX  ~ cauchy(0, priorCauchy);
  sigmaALPHA ~ cauchy(0, priorCauchy);
  sigmaRESP  ~ cauchy(0, priorCauchy);
  sigmaNP    ~ cauchy(0, priorCauchy);

  // Posterior distribution
  for(k in 1:K) {
    segment(pnet, pos, S[k]) ~ student_t(2,segment(pnet_fit, pos, S[k]), sigmaNP[k]);
    pos = pos + S[k];
  }
}
generated quantities {
  vector[M] yhat[K];
  vector[M] ypred[K];
  real log_lik[N]; // Needed to calculate loo stats

  for(k in 1:K){
    yhat[k] = npmodel(PMAX[k], ALPHA[k], RESP[k], light);
  }
  for(m in 1:M)  {
    for(k in 1:K) {
      ypred[k, m] = student_t_rng(2, yhat[k, m], sigmaNP[k]);
    }
  }
  for(n in 1:N) {
    log_lik[n] = normal_lpdf(pnet[n] | npmodel_real(PMAX[idx[n]],
                          ALPHA[idx[n]], 
                          RESP[idx[n]], ppfd[n]), sigmaNP[idx[n]]) ;
  }
}



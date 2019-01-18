// Photosynthesis -- irradiance curve modeling with
// Boltzmann temperature correction. The coefficient
// for excitation energy (EA) is also estimated from
// the data. Note that the coefficients for the
// temperature portion of the model are global
// values, whereas the coefficients for the ppfd
// portion of the model are for each location.

functions {
  vector npmodel (real pmax, real alpha, real rd, vector x) {
    return pmax * (1-exp(-alpha/pmax * x)) - rd;
  }
  real npmodel_real (real pmax, real alpha, real rd, real x) {
    return pmax * (1-exp(-alpha/pmax * x)) - rd;
  }
}
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N; // Number of observations
  int<lower=1> M; // Number of predictions
  int K; // Number of dates
  int idx[N] ; // Index
  int S[K]; // Size of each data set.
  vector<lower=0>[N] ppfd;
  vector<lower=0>[N] temperature;
  vector[N] pnet;
  real priormuPMAXmu;
  real priormuALPHAmu;
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
  real<lower = 0> muPMAX;
  real<lower = 0> muALPHA;
  real muRESP;

  vector<lower = 0>[K] PMAX;
  vector<lower = 0>[K] ALPHA;
  vector[K] RESP;
  real<lower = 0> sigmaPMAX;
  real<lower = 0> sigmaALPHA;
  real<lower = 0> sigmaRESP;

  vector<lower = 0>[K] sigmaNP;
}
transformed parameters {
  vector[N] pnet_fit;
  for(n in 1:N) {
    pnet_fit[n] = npmodel_real(PMAX[idx[n]],
                          ALPHA[idx[n]],
                          RESP[idx[n]], ppfd[n]);
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
    segment(pnet, pos, S[k]) ~ normal(segment(pnet_fit, pos, S[k]), sigmaNP[k]);
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
      ypred[k, m] = normal_rng(yhat[k, m], sigmaNP[k]);
    }
  }
  for(n in 1:N) {
    log_lik[n] = normal_lpdf(pnet[n] | npmodel_real(PMAX[idx[n]],
                          ALPHA[idx[n]],
                          RESP[idx[n]], ppfd[n]), sigmaNP[idx[n]]) ;
  }
}



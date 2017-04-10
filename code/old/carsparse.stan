functions {
  /**
  * Return the log probability of a proper conditional autoregressive (CAR) prior
  * with a sparse representation for the adjacency matrix
  *
  * @param phi Vector containing the parameters with a CAR prior
  * @param tau Precision parameter for the CAR prior (real)
  * @param alpha Dependence (usually spatial) parameter for the CAR prior (real)
  * @param W_sparse Sparse representation of adjacency matrix (int array)
  * @param n Length of phi (int)
  * @param W_n Number of adjacent pairs (int)
  * @param D_sparse Number of neighbors for each location (vector)
  * @param lambda Eigenvalues of D^{-1/2}*W*D^{-1/2} (vector)
  *
  * @return Log probability density of CAR prior up to additive constant
  */
  real sparse_car_lpdf(vector phi, real tau, real alpha,
    int[,] W_sparse, vector D_sparse, vector lambda, int n, int W_n) {
      row_vector[n] phit_D; // phi' * D
      row_vector[n] phit_W; // phi' * W
      vector[n] ldet_terms;

      phit_D = (phi .* D_sparse)';
      phit_W = rep_row_vector(0, n);
      for (i in 1:W_n) {
        phit_W[W_sparse[i, 1]] = phit_W[W_sparse[i, 1]] + phi[W_sparse[i, 2]];
        phit_W[W_sparse[i, 2]] = phit_W[W_sparse[i, 2]] + phi[W_sparse[i, 1]];
      }

      for (i in 1:n) ldet_terms[i] = log1m(alpha * lambda[i]);
      return 0.5 * (n * log(tau)
                    + sum(ldet_terms)
                    - tau * (phit_D * phi - alpha * (phit_W * phi)));
  }
}

data {
  int<lower=0> nobs;
  vector[nobs] y;
  int<lower=0> nx;                  //number of betas
  matrix[nobs, nx] x;               //matrix of covariates, excluding the y-intercept
  real<lower=0> s0;                 //prior mean for sigma
  real<lower=0> vbeta;                //prior variance for beta
  real beta0;                        //prior mean for beta
  matrix[nobs, nobs] W;              //proximity matrix
  int W_n;                          // number of adjacent region pairs
  matrix[nobs, nobs] D;
  real<lower=0> t0;                // priors for tau2
  real<lower=0> sdf;
  real<lower=0> tdf;
  real<lower=0> ar;
  real<lower=0> br;
}

transformed data {
  int W_sparse[W_n, 2];   // adjacency pairs
  vector[nobs] D_sparse;     // diagonal of D (number of neigbors for each site)
  vector[nobs] lambda;       // eigenvalues of invsqrtD * W * invsqrtD
  vector[nobs] invsqrtD;
  { // generate sparse representation for W
  int counter;
  counter = 1;
  // loop over upper triangular part of W to identify neighbor pairs
    for (i in 1:(nobs - 1)) {
      for (j in (i + 1):nobs) {
        if (W[i, j] == 1) {
          W_sparse[counter, 1] = i;
          W_sparse[counter, 2] = j;
          counter = counter + 1;
        }
      }
    }
  }
  for (i in 1:nobs) D_sparse[i] = sum(W[i]);
    for (i in 1:nobs) {
      invsqrtD[i] = 1 / sqrt(D_sparse[i]);
    }
    lambda = eigenvalues_sym(quad_form(W, diag_matrix(invsqrtD)));
}

parameters{
  real<lower=0> sigma;
  vector[nx] beta;
  real<lower=0> tau;
  real<lower=0, upper=1> rho;
  vector[nobs] gamma_un;
}

transformed parameters{
  vector[nobs] gamma;
  gamma = gamma_un - mean(gamma_un);
}

model {
  for (i in 1:nobs) {
    y[i] ~ normal(x[i,]*beta + gamma[i], sigma);
  }
  gamma_un ~ sparse_car(1/tau^2, rho, W_sparse, D_sparse, lambda, nobs, W_n);
  beta ~ normal(beta0, sqrt(vbeta));
  sigma ~ student_t(sdf, 0.0, s0);
  tau ~ student_t(tdf, 0.0, t0);
  rho ~ beta(ar, br);
}

generated quantities {
  vector[nobs] y.mean;
  vector[nobs] y.pred;
  y.mean = x*beta;
  y.pred = y.mean + gamma;
}

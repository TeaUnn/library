# the random walk metropolis sampler samples from any given posterior density.
# proposal density: the multivariate normal density
# with mu = previous theta and sigma = c * cov.

rwmsampler <- function(n,              # number of samples to draw
                       burn = 50,      # size of burn-in
                       c,              # tuning parameter
                       log_posterior,  # log posterior function to simulate from
                       theta_start,    # initial values for parameter(s)
                       cov,            # covariance matrix used in proposal density
                       ...){           # wildcard arguments to send to log_posterior
  library(mvtnorm)
  theta <- matrix(theta_start, nrow = 1)
  rej <- 0
  for(s in 2:(n+burn)){
    theta_prop <- rmvnorm(1,
                          mean = theta[s-1, ],
                          sigma = c * cov)
    r <- exp(log_posterior(c(theta_prop), ...) - log_posterior(c(theta[s-1, ]), ...))
    if(r < runif(1)){
      theta <- rbind(theta, theta[s-1, ])
      rej <- rej + 1
    } else (theta <- rbind(theta, theta_prop))
  }
  samp <- theta[(burn+1):(n+burn), ]
  return(list("sample" = samp, "acceptance rate" = (n+burn-rej)/(n+burn)))
}
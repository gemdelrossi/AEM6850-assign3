
# Assignment 3: Clustered SE's and Monte Carlo Simulation

# 0. Table of Contents ---------------------------------------------------------

# 1. Download and install necessary packages
# 2. Generate data

# 4. Run Monte Carlo simulation
# 5. Generate plot

# 1. ---------------------------------------------------------------------------

# setting up work space

rm(list=ls())
want <- c("RColorBrewer","plm","lfe","sandwich","lmtest","multiwayvcov")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

#2. ----------------------------------------------------------------------------

# generate the starting data for the MC simulation

set.seed(123)
r <-10^3 # monte carlo simulations
g <- c(2) # vector of numbers of clusters

# try and create a loop with vector of n observations

n <- list(100, 200, 500, 10^3, 10^4) # list of number of observations

obs_loop <- lapply(100, function(n) {

  # Generate regression

  b0 <- 1 # coefficient for intercept
  b1 <- .1 # coefficient for x1
  x1 <- rnorm(n) + rep(rnorm(g), each=n/g) # X that is correlated within clusters
  data <- data.frame(x1=x1)

  # Monte Carlo simulation

  out <- lapply(1:r, function(i) {

    print(i)
    n<-100
    # Generate data
    e1 <- rnorm(n) # variance for each obs
    e2 <- rep(rnorm(g), each=n/g) # variance within cluster
    data$clu <- rep(1:g, each=n/g) # cluster id
    e <- e1 + e2 # total error term
    data$y <- b0 + b1*data$x1 + e # dependent variable

    # Run model
    reg <- lm(y~x1, data) # OLS

    # Get coefficients
    b <- coef(reg)

    # Get SEs with different approaches
    se.canned <- sqrt(diag(vcov(reg))) # i.i.d errors
    se.clu    <- sqrt(diag(cluster.vcov(reg, data$clu))) # clustered errors

    # Export
    o <- c(b, se.canned, se.clu)
    names(o) <- paste(rep(c("b.ols","se.ols","se.ols.clu"), each=2), names(o))
    o

  })

  out <- do.call("rbind", out)

  head(out)

  mean(out[,6]) / sd(out[,2])

})









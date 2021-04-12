
# Assignment 3: Clustered SE's and Monte Carlo Simulation


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
n <- c(100, 200, 500, 10^3, 10^4) # sample sizes
g <- c(2,4,5,10,20,25,50,100) # number of clusters

# create empty data frame to fill with final ratio values

df <- data.frame(matrix(ncol = 5, nrow = 8))

# create for loop

for (i in 1:5) { # i is sample sizes (n)
  for(j in 1:8) {# j is clusters (g)

    out <- lapply(1:r, function(x) { #monte carlo simulation

      print(x)

      # Generate regression

      b0 <- 1 # coefficient for intercept
      b1 <- .1 # coefficient for x1
      x1 <- rnorm(n[i]) + rep(rnorm(g[j]), each=n[i]/g[j]) # X correlated within clusters
      data <- data.frame(x1=x1)

      # Generate data

      e1 <- rnorm(n[i]) # variance for each obs
      e2 <- rep(rnorm(g[j]), each=n[i]/g[j]) # variance within cluster
      data$clu <- rep(1:g[j], each=n[i]/g[j]) # cluster id
      e <- e1 + e2 # total error term
      data$y <- b0 + b1*data$x1 + e # dependent variable

      # Run model
      reg <- lm(y~x1, data) # OLS

      # Get coefficients
      b <- coef(reg)

      # Get SE
      se.clu <- sqrt(diag(cluster.vcov(reg, data$clu))) # clustered errors

      # Export
      o <- c(b, se.clu)
      names(o) <- paste(rep(c("b.ols","se.ols.clu"), each=2), names(o))
      o
    })

    out <- do.call("rbind", out)

    # populate ratio data frame

    df[j,i]<- mean(out[,4]) / sd(out[,2]) # SE estimate / std error of sampling dist.
  }
}

df
# each column represents obs 100,200,500,1000,10000
# each row represents clusters 2,4,5,10,20,25,50,100

plot(df[j,], df[,i])












##############################################################################
#UNUSED CODE
##############################################################################

for(w in 2:15) {
  
  #Skip because not enough data
  if(w==9) {
    next
  }
  
  my_data2 <- my_data[,w]
  my_data2 <- data.frame(my_data2)
  
  #Example of how to ignore NA
  #mean(my_data$Developing.Asia,na.rm=TRUE)
  
  # The following is an R implementation of the moving blocks bootstrap
  # following Efron and Tibshirani (sec. 8.6).
  #
  # We create some artificial data:
  N <- nrow(my_data2) # length of the time series
  series <- rnorm(N)                      # initially noise
  series[-1] <- series[-1] + series[-N]   # introduce auto-correlation
  #    
  # Here is the code that collects bootstrap values of
  # the auto-correlation estimate:
  k <- 4                                  # size of moving blocks
  nrep <- 1000                            # number of bootstrap replications
  lag.cor.bt <- rep(NA,nrep)              # vessel for the boostrapped values
  for(irep in 1:nrep) {                   # the bootstrap loop
    series.bt <- rep(NA,N)                # local vector for a bootstrap replication
    for(i in 1:ceiling(N/k)) {            # fill the vector with random blocks
      endpoint <- sample(k:N, size=1)     # by randomly sampling endpoints
      series.bt[(i-1)*k+1:k] <- lapply(my_data2[endpoint-(k:1)+1,], as.numeric) # and copying blocks
      series.bt <- unlist(series.bt, recursive=FALSE)
    }
    series.bt <- series.bt[1:N]           # trim overflow when k doesn't divide N
    lag.cor.bt[irep] <- cor(series.bt[-1],series.bt[-N])  # the auto-cor. estimate
    #if(irep%%50==0) print(irep)           # report every 50 bootstrap passes
    
  }
  #
  # Now that we have a bootstrap distribution for the statistic of interest
  # in lag.cor.bt, we analyze it.
  # First visually:
  hist(lag.cor.bt, col="gray", ncl=20, main = names(my_data[w]))
  #
  # Then numerically:
  # 1) the significance level in terms of the quantile of the value
  #    that represents a null assumption (zero in this case):
  print(sum(lag.cor.bt<0,na.rm=TRUE)/1000)
  #
  # 2) a 95% quantile confidence interval:
  print(quantile(lag.cor.bt, c(0.025,0.975),na.rm=TRUE))
}

###############################################################################

bootseries <- bld.mbb.bootstrap(my_data$Developing.Asia, 100) %>%
  as.data.frame() %>% ts(start=0, frequency=1)
autoplot(ts(my_data$Developing.Asia)) +
  autolayer(bootseries, colour=TRUE) +
  autolayer(ts(my_data$Developing.Asia), colour=FALSE) +
  ylab("Bootstrapped series") + guides(colour="none")

###############################################################################

nsim <- 1000L
sim <- bld.mbb.bootstrap(ts(my_data$Developing.Asia), nsim)

h <- 36L
future <- matrix(0, nrow=nsim, ncol=h)
for(i in seq(nsim)) {
  future[i,] <- simulate(ets(sim[[i]]), nsim=h)
  print(i)
}

start <- tsp(ts(my_data$Developing.Asia))[2]
simfc <- structure(list(
  mean = ts(colMeans(future), start=start, frequency=1),
  lower = ts(apply(future, 2, quantile, prob=0.05),
             start=start, frequency=1),
  upper = ts(apply(future, 2, quantile, prob=0.95),
             start=start, frequency=1),
  level=90),
  class="forecast")

etsfc <- forecast(ets(ts(my_data$Developing.Asia)), h=h, level=90)
autoplot(ts(my_data$Developing.Asia)) +
  ggtitle("Developing.Asia") +
  xlab("Month") + ylab("Consumer Price Index") +
  autolayer(simfc, series="Simulated") +
  autolayer(etsfc, series="ETS")

###############################################################################

etsfc <- ts(my_data$Developing.Asia) %>% ets() %>% forecast(h=36)
baggedfc <- ts(my_data$Developing.Asia) %>% baggedETS() %>% forecast(h=36)
autoplot(ts(my_data$Developing.Asia)) +
  autolayer(baggedfc, series="BaggedETS", PI=FALSE) +
  autolayer(etsfc, series="ETS", PI=FALSE) +
  guides(colour=guide_legend(title="Forecasts"))

###############################################################################



###############################################################################
#CODE WE ARE USING
###############################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(fpp2)

setwd("/Users/mryvi/Documents/ComputerGraphStats")
set.seed(538)

#Excel file was renamed
#MAKE SURE TO USE MONTHLY DATASET
my_data <- read_excel("CPIPrice.xlsx")
#First row (empty) removed
my_data <- my_data[-1,]
#Last row (NA) removed
my_data <- my_data[-360,]

my_data <- data.frame(lapply(my_data, as.numeric))

for(w in 2:15) {
  
  start <- tsp(ts(my_data[[w]],frequency = 12))[2]-1
  
  #Skip because not enough data
  if(w==9) {
    next
  }

  sim <- bld.mbb.bootstrap(ts(my_data[[w]],frequency = 12), 10) %>%
    as.data.frame() %>%
    ts(frequency=12, start=1989.5)
  fc <- purrr::map(as.list(sim),
                   function(x){forecast(ets(x))[["mean"]]}) %>%
    as.data.frame() %>%
    ts(frequency=12, start=start+1989.5)
  print(autoplot(ts(my_data[[w]],frequency = 12,start=1989.5)) +
    autolayer(sim, colour=TRUE) +
    autolayer(fc, colour=TRUE) +
    autolayer(ts(my_data[[w]],frequency = 12,start=1989.5), colour=FALSE) +
    ylab("Consumer Price Index") +
    guides(colour="none") + ggtitle(names(my_data[w])) )

  cat("The average predicted value for", names(my_data3[w]), "(1989-2019) is:", mean(fc), "\n"  )
  cat("The standard deviation of the predicted value for", names(my_data3[w]), "(1989-2019) is:", sd(fc) )
}

#10 years ago to latest point in data, aka April 2012 - April 2019
my_data2 <- my_data[275:359,]

for(w in 2:15) {
  
  start <- tsp(ts(my_data2[[w]],frequency = 12))[2]-1
  
  #Skip because not enough data
  if(w==9) {
    next
  }
  
  sim <- bld.mbb.bootstrap(ts(my_data2[[w]],frequency = 12), 10) %>%
    as.data.frame() %>%
    ts(frequency=12, start=2012.333)
  fc <- purrr::map(as.list(sim),
                   function(x){forecast(ets(x))[["mean"]]}) %>%
    as.data.frame() %>%
    ts(frequency=12, start=start+2012.333)
  print(autoplot(ts(my_data2[[w]],frequency = 12,start=2012.333)) +
          autolayer(sim, colour=TRUE) +
          autolayer(fc, colour=TRUE) +
          autolayer(ts(my_data2[[w]],frequency = 12,start=2012.333), colour=FALSE) +
          ylab("Consumer Price Index") +
          guides(colour="none") + ggtitle(names(my_data2[w])) )
  
  cat("The average predicted value for", names(my_data3[w]), "(2012-2019) is:", mean(fc), "\n"  )
  cat("The standard deviation of the predicted value for", names(my_data3[w]), "(2012-2019) is:", sd(fc) )
}

#5 years ago to latest point in data, aka April 2017 - April 2019
my_data3 <- my_data[335:359,]

for(w in 2:15) {
  
  start <- tsp(ts(my_data3[[w]],frequency = 12))[2]-1
  
  #Skip because not enough data
  if(w==9) {
    next
  }
  
  sim <- bld.mbb.bootstrap(ts(my_data3[[w]],frequency = 12), 10) %>%
    as.data.frame() %>%
    ts(frequency=12, start=2017.333)
  fc <- purrr::map(as.list(sim),
                   function(x){forecast(ets(x))[["mean"]]}) %>%
    as.data.frame() %>%
    ts(frequency=12, start=start+2017.333)
  print(autoplot(ts(my_data3[[w]],frequency = 12,start=2017.333)) +
          autolayer(sim, colour=TRUE) +
          autolayer(fc, colour=TRUE) +
          autolayer(ts(my_data3[[w]],frequency = 12,start=2017.333), colour=FALSE) +
          ylab("Consumer Price Index") +
          guides(colour="none") + ggtitle(names(my_data3[w])) )
  cat("The average predicted value for", names(my_data3[w]), "(2017-2019) is:", mean(fc), "\n" )
  cat("The standard deviation of the predicted value for", names(my_data3[w]), "(2017-2019) is:", sd(fc) )
  
}


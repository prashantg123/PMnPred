library(TTR)

Preprocess <- function(points, current, patterns, interval, period) {
  # Preprocess 6 months data before current time to predict
  begin <- current - 60/interval * 24 * 125 - 168
  begin <- ifelse(begin < period/interval+1, period/interval+1, begin) 
  k <- tail(which(patterns$t1.x <= begin), 1) #start row of patterns > begin
  l <- head(which(patterns$t3.x >= current), 1) # end row of patterns 
    
  training <- NULL
  # Mark each point in the range the pattern it belongs to
  if (length(k) == 0) {
    k <- 1
    for (i in begin:(patterns$t1.x[1] - 1)) {
      single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
      if (is.data.frame(training)) {
        training <- rbind(training, single)
      } else {
        training <- single
      }
    }
  } 
  
  if (length(l) == 0) {
    l <- nrow(patterns)
  }
  
  for (p in k:l) {
    for (i in patterns$t1.x[p] : (patterns$t2.x[p] - 1)) {
      if (patterns$t2.y[p] - patterns$t1.y[p] > 0) {
        single <- data.frame(index = i, pattern = "moving up", dt1 = i - patterns$t1.x[p], 
                             dt2 = patterns$t2.x[p] - i, dt3 = patterns$t3.x[p] - i,
                             dy2 = patterns$t2.y[p] - points[i], dy3 = NA)
      } else {
        single <- data.frame(index = i, pattern = "moving down", dt1 = i - patterns$t1.x[p], 
                             dt2 = patterns$t2.x[p] - i, dt3 = patterns$t3.x[p] - i,
                             dy2 = patterns$t2.y[p] - points[i], dy3 = NA)
      }
      
      if (is.data.frame(training)) {
        training <- rbind(training, single)
      } else {
        training <- single
      }
    }
    
    for (i in patterns$t2.x[p] : (patterns$t3.x[p] - 1)) {
      if (patterns$t3.y[p] - patterns$t2.y[p] > 0) {
        single <- data.frame(index = i, pattern = "upward retracement", dt1 = i - patterns$t1.x[p], 
                             dt2 = i - patterns$t2.x[p], dt3 = patterns$t3.x[p] - i, 
                             dy2 = NA, dy3 = patterns$t3.y[p] - points[i])
      } else {
        single <- data.frame(index = i, pattern = "downward retracement", dt1 = i - patterns$t1.x[p], 
                             dt2 = i - patterns$t2.x[p], dt3 = patterns$t3.x[p] - i, 
                             dy2 = NA, dy3 = patterns$t3.y[p] - points[i])
      }
      
      training <- rbind(training, single)
    }
    
    if (p < l) {
      if (patterns$t3.x[p] < patterns$t1.x[p+1]) { 
        for (i in patterns$t3.x[p] : (patterns$t1.x[p+1] - 1)) {
          single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
          training <- rbind(training, single)
        }
      }
    }
  }
  
  single <- data.frame(index = patterns$t3.x[l], pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
  training <- rbind(training, single)

  if (tail(patterns$t3.x, 1) < current ) {
    for (i in (patterns$t3.x[l] + 1) : current) {
      single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
      training <- rbind(training, single)
    }
  }

  
  # Calculate each point in the range its slope, volatility, SMA
  training$price <- rep(NA, nrow(training))
  training$slope <- rep(NA, nrow(training))
  training$volatility <- rep(NA, nrow(training))
  training$SMA <- rep(NA, nrow(training))
  training$pctB <- rep(NA, nrow(training))
  training$DEMA <- rep(NA, nrow(training))
  
  sma <- SMA(points, n = period / interval)
  bbands <- BBands(points, n = period / interval, maType = SMA)
  pctB <- bbands[, "pctB"]
  dema <- DEMA(points, n = period / interval)
  
  for (i in 1:nrow(training)) {
    training$price[i] <- points[training$index[i]]
    training$slope[i] <- points[training$index[i]] - points[training$index[i] - 1]
    training$volatility[i] <- sd(points[(training$index[i] - period/interval) : 
                                          training$index[i]]) / (period/interval)  # volatility during the past 1 hour
    training$SMA[i] <- sma[training$index[i]]
    training$pctB[i] <- pctB[training$index[i]]
    training$DEMA[i] <- dema[training$index[i]]
  }
  
  training[order(training$index), ] #order output the index of value
}



Delta <- function(HFdata, points, current, patterns, interval, period) {
  # Preprocess 6 months data before current time to predict
  begin <- current - 60/interval * 24 * 125 - 168
  
  if (begin %in% HFdata$index && current %in% HFdata$index) {
    return (HFdata)
  }
  
  if (!(begin %in% HFdata$index) && !(current %in% HFdata$index)) {
    training <- Preprocess(points, current, patterns, interval, period)  
  }
  
  if (begin %in% HFdata$index && !(current %in% HFdata$index)) { 
    
    margin <- tail(HFdata$index[(HFdata$index + HFdata$dt3) < current], 1)
    k <- which(patterns$t1.x == margin)

    training <- NULL
    # Mark each point in the range the pattern it belongs to
    if (length(k) == 0) {
      k <- head(which(patterns$t1.x > margin), 1)
      
      if (patterns$t1[k] %in% (HFdata$index - HFdata$dt1) || length(k) == 0) {
        for (i in margin : current) {
          single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
          if (is.data.frame(training)) {
            training <- rbind(training, single)
          } else {
            training <- single
          }
        }
        HFdata <- rbind(HFdata, training)
        HFdata <- HFdata[order(HFdata$index), ]
        return(HFdata)
      }

      for (i in margin:(patterns$t1.x[k] - 1)) {
        single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
        if (is.data.frame(training)) {
          training <- rbind(training, single)
        } else {
          training <- single
        }
      }
    }
    
    l <- head(which(patterns$t3.x >= current), 1)
    if (patterns$t1[l] %in% (HFdata$index - HFdata$dt1)) {
      l <- l-1
    }

    if (length(l) == 0) {
      l <- nrow(patterns)
    }
    
    for (p in k:l) {
      for (i in patterns$t1.x[p] : (patterns$t2.x[p] - 1)) {
        if (patterns$t2.y[p] - patterns$t1.y[p] > 0) {
          single <- data.frame(index = i, pattern = "moving up", dt1 = i - patterns$t1.x[p], 
                               dt2 = patterns$t2.x[p] - i, dt3 = patterns$t3.x[p] - i,
                               dy2 = patterns$t2.y[p] - points[i], dy3 = NA)
        } else {
          single <- data.frame(index = i, pattern = "moving down", dt1 = i - patterns$t1.x[p], 
                               dt2 = patterns$t2.x[p] - i, dt3 = patterns$t3.x[p] - i,
                               dy2 = patterns$t2.y[p] - points[i], dy3 = NA)
        }
        
        if (is.data.frame(training)) {
          training <- rbind(training, single)
        } else {
          training <- single
        }
      }
      
      for (i in patterns$t2.x[p] : (patterns$t3.x[p] - 1)) {
        if (patterns$t3.y[p] - patterns$t2.y[p] > 0) {
          single <- data.frame(index = i, pattern = "upward retracement", dt1 = i - patterns$t1.x[p], 
                               dt2 = i - patterns$t2.x[p], dt3 = patterns$t3.x[p] - i, 
                               dy2 = NA, dy3 = patterns$t3.y[p] - points[i])
        } else {
          single <- data.frame(index = i, pattern = "downward retracement", dt1 = i - patterns$t1.x[p], 
                               dt2 = i - patterns$t2.x[p], dt3 = patterns$t3.x[p] - i, 
                               dy2 = NA, dy3 = patterns$t3.y[p] - points[i])
        }
        
        training <- rbind(training, single)
      }
      
      if (p < l) {
        if (patterns$t3.x[p] < patterns$t1.x[p+1]) {
          for (i in patterns$t3.x[p] : (patterns$t1.x[p+1] - 1)) {
            single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
            training <- rbind(training, single)
          }
        }
      }
    }
    
    single <- data.frame(index = patterns$t3.x[l], pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
    training <- rbind(training, single)
    
    if (patterns$t3.x[l] < current ) {
      for (i in (patterns$t3.x[l] + 1) : current) {
        single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
        training <- rbind(training, single)
      }
    }
    
    
    # Calculate each point in the range its slope, volatility, SMA
    training$price <- rep(NA, nrow(training))
    training$slope <- rep(NA, nrow(training))
    training$volatility <- rep(NA, nrow(training))
    training$SMA <- rep(NA, nrow(training))
    training$pctB <- rep(NA, nrow(training))
    training$DEMA <- rep(NA, nrow(training))
    
    sma <- SMA(points, n = period / interval)
    bbands <- BBands(points, n = period / interval, maType = SMA)
    pctB <- bbands[, "pctB"]
    dema <- DEMA(points, n = period / interval)
    
    for (i in 1:nrow(training)) {
      training$price[i] <- points[training$index[i]]
      training$slope[i] <- points[training$index[i]] - values[start + training$index[i] - 2]
      training$volatility[i] <- sd(values[(start + training$index[i] - 1 - period/interval) : 
                                            (start + training$index[i] - 1)]) / (period/interval)  # volatility during the past 1 hour
      training$SMA[i] <- sma[training$index[i]]
      training$pctB[i] <- pctB[training$index[i]]
      training$DEMA[i] <- dema[training$index[i]]
    } 
  }
  
  if (!(begin %in% HFdata$index) && current %in% HFdata$index) { 
    
    margin <- HFdata$index[(HFdata$index - HFdata$dt1) > begin][1] - 1
    
    k <- tail(which(patterns$t1.x <= begin), 1)
    
    l <- which(patterns$t3.x >= margin)[1]
    if (patterns$t1[l] %in% (HFdata$index - HFdata$dt1)) {
      l <- l-1
    }
    
    training <- NULL
    if (l == 0) {
      for (i in begin : margin) {
        single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
        if (is.data.frame(training)) {
          training <- rbind(training, single)
        } else {
          training <- single
        }
      }
      HFdata <- rbind(HFdata, training)
      HFdata <- HFdata[order(HFdata$index), ]
      return(HFdata)
    }
    
    # Mark each point in the range the pattern it belongs to
    if (length(k) == 0) {
      k <- 1
      for (i in 1:(patterns$t1.x[1] - 1)) {
        single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
        if (is.data.frame(training)) {
          training <- rbind(training, single)
        } else {
          training <- single
        }
      }
    } 
     
    for (p in k:l) {
      for (i in patterns$t1.x[p] : (patterns$t2.x[p] - 1)) {
        if (patterns$t2.y[p] - patterns$t1.y[p] > 0) {
          single <- data.frame(index = i, pattern = "moving up", dt1 = i - patterns$t1.x[p], 
                               dt2 = patterns$t2.x[p] - i, dt3 = patterns$t3.x[p] - i, 
                               dy2 = patterns$t2.y[p] - points[i], dy3 = NA)
        } else {
          single <- data.frame(index = i, pattern = "moving down", dt1 = i - patterns$t1.x[p], 
                               dt2 = patterns$t2.x[p] - i, dt3 = patterns$t3.x[p] - i, 
                               dy2 = patterns$t2.y[p] - points[i], dy3 = NA)
        }
        
        if (is.data.frame(training)) {
          training <- rbind(training, single)
        } else {
          training <- single
        }
      }
      
      for (i in patterns$t2.x[p] : (patterns$t3.x[p] - 1)) {
        if (patterns$t3.y[p] - patterns$t2.y[p] > 0) {
          single <- data.frame(index = i, pattern = "upward retracement", dt1 = i - patterns$t1.x[p], 
                               dt2 = i - patterns$t2.x[p], dt3 = patterns$t3.x[p] - i, 
                               dy2 = NA, dy3 = patterns$t3.y[p] - points[i])
        } else {
          single <- data.frame(index = i, pattern = "downward retracement", dt1 = i - patterns$t1.x[p], 
                               dt2 = i - patterns$t2.x[p], dt3 = patterns$t3.x[p] - i, 
                               dy2 = NA, dy3 = patterns$t3.y[p] - points[i])
        }
        
        training <- rbind(training, single)
      }
      
      if (p < l) {
        if (patterns$t3.x[p] < patterns$t1.x[p+1]) {
          for (i in patterns$t3.x[p] : (patterns$t1.x[p+1] - 1)) {
            single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
            training <- rbind(training, single)
          }
        }
      }
    }
    
    single <- data.frame(index = patterns$t3.x[l], pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
    training <- rbind(training, single)
    
    if (patterns$t3.x[l] < margin) {
      for (i in (patterns$t3.x[l] + 1) : margin) {
        single <- data.frame(index = i, pattern = "none", dt1 = NA, dt2 = NA, dt3 = NA, dy2 = NA, dy3 = NA)
        training <- rbind(training, single)
      }
    }
    
    
    # Calculate each point in the range its slope, volatility, SMA
    training$price <- rep(NA, nrow(training))
    training$slope <- rep(NA, nrow(training))
    training$volatility <- rep(NA, nrow(training))
    training$SMA <- rep(NA, nrow(training))
    training$pctB <- rep(NA, nrow(training))
    training$DEMA <- rep(NA, nrow(training))
    
    sma <- SMA(points, n = period / interval)
    bbands <- BBands(points, n = period / interval, maType = SMA)
    pctB <- bbands[, "pctB"]
    dema <- DEMA(points, n = period / interval)
    
    for (i in 1:nrow(training)) {
      training$price[i] <- points[training$index[i]]
      training$slope[i] <- points[training$index[i]] - values[start + training$index[i] - 2]
      training$volatility[i] <- sd(values[(start + training$index[i] - 1 - period/interval) : 
                                            (start + training$index[i] - 1)]) / (period/interval)  # volatility during the past 1 hour
      training$SMA[i] <- sma[training$index[i]]
      training$pctB[i] <- pctB[training$index[i]]
      training$DEMA[i] <- dema[training$index[i]]
    } 
  }

  HFdata <- rbind(HFdata, training)
  HFdata <- HFdata[order(HFdata$index), ]
  HFdata
}
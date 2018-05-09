library(e1071)
library(randomForest)
#library(caret)
library(gridExtra)

PredictTest <- function(HFdata, current, time.index) {
  
  HFdata$pattern=factor(HFdata$pattern, levels = c("moving up", "downward retracement", 
                                                   "moving down", "upward retracement", 
                                                   "none"))
  
  ####Generate predictors for each index pt
  stepback <- function(n, x) {
    a=rep(NA,n)
    result=c(a,x[1:(length(x)-n)])  
    return(result)
  }
  
  
  SMA=HFdata$SMA
  SMA.min1=stepback(1,SMA)
  SMA.min2=stepback(2,SMA)
  SMA.min3=stepback(3,SMA)
  SMA.min4=stepback(4,SMA)
  SMA.min5=stepback(5,SMA)
  HFdata=data.frame(HFdata, SMA.min1, SMA.min2, SMA.min3, SMA.min4, SMA.min5)
  
  
  slope=HFdata$slope
  slope.min1=stepback(1,slope)
  slope.min2=stepback(2,slope)
  slope.min3=stepback(3,slope)
  slope.min4=stepback(4,slope)
  slope.min5=stepback(5,slope)
  HFdata=data.frame(HFdata, slope.min1, slope.min2, slope.min3, slope.min4, slope.min5)
  
  
  vol=HFdata$volatility
  vol.min1=stepback(1,vol)
  vol.min2=stepback(2,vol)
  vol.min3=stepback(3,vol)
  vol.min4=stepback(4,vol)
  vol.min5=stepback(5,vol)
  HFdata=data.frame(HFdata, vol.min1, vol.min2, vol.min3, vol.min4, vol.min5)
  
  
  DEMA=HFdata$DEMA
  DEMA.min1=stepback(1,DEMA)
  DEMA.min2=stepback(2,DEMA)
  DEMA.min3=stepback(3,DEMA)
  DEMA.min4=stepback(4,DEMA)
  DEMA.min5=stepback(5,DEMA)
  HFdata=data.frame(HFdata, DEMA.min1, DEMA.min2, DEMA.min3, DEMA.min4, DEMA.min5)

  
  ##remove variables

  HFdata$pctB=NULL

  
  HFdata2 <- HFdata
  HFdata2$dt1=NULL
  HFdata2$dt2=NULL
  HFdata2$dt3=NULL
  HFdata2$dy2=NULL
  HFdata2$dy3=NULL
 
  ## Predict pattern type of the next point
  begin <- current - 60/interval * 24 * 125
  begin <- ifelse(begin < period/interval+20, period/interval+20, begin)
  k <- match(begin, HFdata$index) # Previous 6 month to current as training data
  l <- match(current, HFdata$index)
  
  train.subset= k : (l - 1)
  
  if (trend == "up") {
    type <- "downward retracement"
  } else {
    type <- "upward retracement"
  }

  #Group training data for t3 of downward retracement pattern
  train.t3 <- NULL
  for (j in train.subset) {
    if (HFdata$pattern[j] == type) {
      if (is.data.frame(train.t3)) {
        train.t3 <- rbind(train.t3, HFdata[j, ])
      } else {
        train.t3 <- HFdata[j, ]
      }
    }
  }
  
  train.t3$dy2 <- NULL
  
  # Generate estimated dt1 dt2 and row record for t3 prediction
  matched <- FindT1T2(current)

  if (!is.data.frame(matched)) {
    current.table <- tableGrob("No t1 t2 found!")
    move.table <- tableGrob("No t1 t2 found!")
    
    first <- ifelse(current-12*12<1, 1, current-12*12)
    last <- ifelse(current+12*12>length(points), length(points), current+12*12)
    
    df <- data.frame(x = first:last, y = points[first:last])
    llim <- min(df$y)
    ulim <- max(df$y)
    
    p <- ggplot(df, aes(x = x, y = y)) + geom_line() + 
      theme(axis.text.x = element_text(angle=90)) +
      scale_y_continuous(breaks = round(seq(llim, ulim, by = 0.02), 2), 
                         limits = c(llim, ulim)) +
      scale_x_continuous(breaks = seq(1, length(time.index), by = 24), 
                         labels = as.character(time.index[seq(1, length(time.index), by = 24)]))
    p <- p + geom_text(x=current, y=points[current], size=5, colour="red", label="t", hjust=0, vjust=0)
  } else {
    pt1 <- matched$t1.x
    pt2 <- matched$t2.x
    pdt1 <- current - pt1
    pdt2 <- current - pt2
    volatility12 <- sd(points[pt1 : pt2]) / (pt2 - pt1) * 100
    
    row <- HFdata[l, ]
    t1 <- current - row$dt1
    
    if (row$pattern == "moving up" || row$pattern == "moving down") {
      t2 <- current + row$dt2
    }
    if (row$pattern == "downward retracement" || row$pattern == "upward retracement") {
      t2 <- current - row$dt2
    }
    
    t3 <- current + row$dt3
    row$dt1 <- pdt1
    row$dt2 <- pdt2
    
    if (row$pattern == "none") {
      row$dt3 <- 0
    }
    
    ######
    ######6 index pts back
    ##Estimate SVM, Random Forest models
    if (is.na(row$dy3)){
      row$dy3 <- 2
    }
    
    t3.rf.fit=randomForest(dt3~.-index-pattern-price-dy3, data=train.t3, ntree=1000,
                           importance=TRUE)
    pdt3 <- predict(t3.rf.fit, row)
    
    pt3 <- current + pdt3
    
    dy3.rf.fit=randomForest(dy3~.-index-pattern-dt1-dt2-price, data=train.t3, ntree=1000,
                            importance=TRUE)
    row$dt3 <- pdt3
    pdy3 <- predict(dy3.rf.fit, row)
    py3 <- points[current] + pdy3
    
    patterns <- FindAll(points[1:current], interval = interval, 
                        min.basis = min.basis, move.duration = move.duration, 
                        noise.basis = noise.basis, noise.duration = noise.duration, 
                        retrace.percent = retrace.percent, 
                        retrace.min = retrace.min, retrace.duration = retrace.duration)
    
    if (is.data.frame(patterns)) {
      npatterns <- nrow(patterns)
      patterns2 <- patterns
      patterns2$t1.x <- time.index[patterns2$t1.x]
      patterns2$t2.x <- time.index[patterns2$t2.x]
      patterns2$t3.x <- time.index[patterns2$t3.x]
      patterns2$t1.y <- format(round(patterns2$t1.y, 3), nsmall = 3)
      patterns2$t2.y <- format(round(patterns2$t2.y, 3), nsmall = 3)
      patterns2$t3.y <- format(round(patterns2$t3.y, 3), nsmall = 3)
      patterns2$t12 <- patterns2$t12 * interval
      patterns2$t23 <- patterns2$t23 * interval
      patterns2$vol12 <- format(round(patterns2$vol12, 4), nsmall = 4)
      patterns2$vol23 <- format(round(patterns2$vol23, 4), nsmall = 4)
      patterns2$dy21 <- format(round(patterns2$dy21*100, 2), nsmall = 2)
      patterns2$dy32 <- format(round(patterns2$dy32*100, 2), nsmall = 2)
      patterns2$retrace.pct <- sapply(patterns2$retrace.pct, Percent)
      colnames(patterns2) <- c("t1 Time", "t1 Price", "t2 Time", "t2 Price", "t3 Time", "t3 Price", "Move Time", 
                               "Retrace Time", "Move Vol", "Retrace Vol", "Move BPs", 
                               "Retrace BPs", "Retrace %")
      
      write.csv(patterns2, paste0(params$app.path,"/output/patterns.csv"), quote=FALSE)
    } else {
      npatterns <- 0
      write.csv(NULL, paste0(params$app.path,"/output/patterns.csv"), quote=FALSE)
    }
    
    if (type == "downward retracement") {
      
      patterns.sub <- patterns[patterns$dy21>0, ]
      
    } else if (type == "upward retracement") {
      
      patterns.sub <- patterns[patterns$dy21<0, ]
    } 
    
    patterns.filtered <- patterns.sub[abs(patterns.sub$t12 - (pt2-pt1)) < 30 & abs(patterns.sub$dy21 - (points[pt2] - points[pt1])) < 0.01
                                      & abs(patterns.sub$vol12 - volatility12) < 0.005, ]  # Put more filtering
    
    if (nrow(patterns.filtered) == 0 || length(patterns.filtered) == 0) {
      nfiltered <- 0
      move.table <- "No historic pattern matched!"
      
    } else {
      nfiltered <- nrow(patterns.filtered)
      dy32 <- patterns.filtered$dy32 *100
      breaks = seq(floor(min(dy32)), ceiling(max(dy32)), by=1)
      dy32.cut <- cut(dy32, breaks, right=FALSE)
      dy32.freq <- table(dy32.cut)
      dy32.sort <- sort(dy32.freq, decreasing=TRUE)
      
      pt23 <- rep(NA, ifelse(length(dy32.sort)>3, 3, length(dy32.sort)))
      for (j in 1:ifelse(length(dy32.sort)>3, 3, length(dy32.sort))) {
        t23 <- patterns.filtered$t23[abs(patterns.filtered$dy32 *100 - (floor(min(dy32)) + order(dy32.freq, decreasing=TRUE)[j] - 0.5)) < 0.5]
        skew <- skewness(t23)
        if (is.na(skew)) {
          pt23[j] <- t23
        } else {
          pt23[j] <- ifelse(skew>0, summary(t23)[5], summary(t23)[2])
        }
        
      }
      
      move.table <- as.data.frame(dy32.sort[1:length(dy32.sort)] / length(dy32))
      move.table[1] <- sapply(move.table[1], Percent)
      move.table$move <- row.names(move.table)
      move.table <- move.table[1:length(pt23), ]
      move.table$t23 <- pt23 * interval
      colnames(move.table) <- c("Retrace Prob.", "Basis Points", "in Mins")
      rownames(move.table) <- NULL
    }
    
    move.table <- tableGrob(move.table)    
    
    
    if (is.na(t1)) {
      first <- pt1-24
      last <- pt3+120
    } else {
      first <- ifelse(t1<pt1, t1-24, pt1-24)
      last <- ifelse(t3>pt3, t3+120, pt3+120)
    }
    first <- ifelse(first<1, 1, first)
    last <- ifelse(last>length(points), length(points), last)
    
    df <- data.frame(x = first:last, y = points[first:last])
    
    llim <- ifelse(min(df$y)<py3, min(df$y), py3)
    ulim <- ifelse(max(df$y)>py3, max(df$y), py3)
    angle <- atan(pdy3 * (last - first) / pdt3 / (ulim - llim)) * 180 / pi
    
    p <- ggplot(df, aes(x = x, y = y)) + geom_line() + 
      theme(axis.text.x = element_text(angle=90)) +
      scale_y_continuous(breaks = round(seq(llim, ulim, by = 0.02), 2), 
                         limits = c(llim, ulim)) +
      scale_x_continuous(breaks = seq(1, length(time.index), by = 24), 
                         labels = as.character(time.index[seq(1, length(time.index), by = 24)])) 
    
    p <- p + geom_text(x=current, y=points[current], size=5, colour="red", label="t", hjust=0, vjust=0)
    
    p <- p + geom_text(x=pt1, y=points[pt1], size=5, colour="blue", label="pt1", hjust=0, vjust=0)
    p <- p + geom_text(x=pt2, y=points[pt2], size=5, colour="blue", label="pt2", hjust=0, vjust=0)
    p <- p + geom_text(x=pt3, y=py3, size=5, colour="blue", label="pt3", hjust=0, vjust=0)
    
    if (!is.na(t1)) {
      p <- p + geom_text(x=t1, y=points[t1]-0.0025, size=5, colour="green", label="t1", hjust=0, vjust=0)
      p <- p + geom_text(x=t2, y=points[t2], size=5, colour="green", label="t2", hjust=0, vjust=0)
      p <- p + geom_text(x=t3, y=points[t3], size=5, colour="green", label="t3", hjust=0, vjust=0)
    }
    
    current.table <- list()
    current.table$pt1 <- time.index[pt1]
    current.table$pt1.y <- format(round(points[pt1], 3), nsmall = 3)
    current.table$pt2 <- time.index[pt2]
    current.table$pt2.y <- format(round(points[pt2], 3), nsmall = 3)
    current.table$dt <- (pt2 - pt1) * interval
    current.table$dy <- format(round((points[pt2] - points[pt1])*100, 2), nsmall = 2)
    current.table$angle <- round(atan(9/20*(points[pt2] - points[pt1]) * (last - first) / (pt2 - pt1) / (ulim - llim)) * 180 / pi)
    current.table$vol <- format(round(volatility12, 4), nsmall = 4)
    current.table <- as.data.frame(current.table)
    rownames(current.table) <- "1"
    colnames(current.table) <- c("t1 Time", "t1 Price", "t2 Time", "t2 Price", "Move Time", 
                                 "Move BPs", "Move Angle", "Move Vol")
    
    current.table <- tableGrob(current.table)
  }
    
  grid.newpage()
  vp1 <- viewport(width = 0.75, height = 0.95, x = 0.375, y = 0.45)
  subvp <- viewport(width = 0.3, height = 0.3, x = 0.875, y = 0.5)
  rightvp <- viewport(width = 0.3, height = 0.3, x = 0.875, y = 0.75)
  resultvp <- viewport(width = 0.3, height = 0.3, x = 0.875, y = 0.25)
  upvp <- viewport(width = 0.3, height = 0.3, x = 0.375, y = 0.99)
  downvp <- viewport(width = 0.3, height = 0.3, x = 0.375, y = 0.935)
  print(p, vp = vp1)
  
  upViewport(0)
  pushViewport(subvp)
  grid.draw(move.table)
  
  upViewport(0)
  pushViewport(upvp)
  grid.draw(tableGrob(paste("Prediction Time  ", time.index[current])))
  
  upViewport(0)
  pushViewport(downvp)
  grid.draw(current.table)
  
  upViewport(0)
  pushViewport(rightvp)
  grid.draw(tableGrob(paste(npatterns, "total historic patterns found\n", 
                            nfiltered, "matched")))
  upViewport(0)
  pushViewport(resultvp)
  grid.draw(tableGrob(paste("Retrace", format(round(pdy3*100, 2), nsmall = 2), "bps in",    
                            round(pdt3*interval), "mins", "(", round(angle), "deg )")))
}

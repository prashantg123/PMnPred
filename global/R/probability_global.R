library(randomForest)
library(e1071)
library(ggplot2)
library(gridExtra)

PredictProb <- function(current, time.index) {
  
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
    
    if (trend == "up") {
      patterns.sub <- patterns[patterns$dy21>0, ]
    } else if (trend == "down") {

      patterns.sub <- patterns[patterns$dy21<0, ]
    } 

    patterns.filtered <- patterns.sub[abs(patterns.sub$t12 - (pt2-pt1)) < 40 & abs(patterns.sub$dy21 - (points[pt2] - points[pt1])) < 0.02
                                           & abs(patterns.sub$vol12 - volatility12) < 0.02, ]  # Put more filtering
    
    if (nrow(patterns.filtered) == 0 || length(patterns.filtered) == 0) {
      nfiltered <- 0
      move.table <- "No historic pattern matched!"
      
    } else {
      nfiltered <- nrow(patterns.filtered)
      dy32 <- patterns.filtered$dy32 *100
      breaks = seq(floor(min(dy32)), ceiling(max(dy32)), by=1)
      dy32.cut <- cut(dy32, breaks, right=FALSE) # give range in which dy32 value fall in
      dy32.freq <- table(dy32.cut)
      dy32.sort <- sort(dy32.freq, decreasing=TRUE)
      
      pt23 <- rep(NA, ifelse(length(dy32.sort)>3, 3, length(dy32.sort)))
      for (j in 1:ifelse(length(dy32.sort)>3, 3, length(dy32.sort))) {
        t23 <- patterns.filtered$t23[abs(patterns.filtered$dy32 *100 - (floor(min(dy32)) + order(dy32.freq, decreasing=TRUE)[j] - 0.5)) < 0.5] # order give value to hihg to low
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
    
    first <- ifelse(pt1-24<1, 1, pt1-24)
    last <- ifelse(current+60/interval*12>length(points), length(points), current+60/interval*12)
    
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
    
    p <- p + geom_text(x=pt1, y=points[pt1], size=5, colour="blue", label="pt1", hjust=0, vjust=0)
    p <- p + geom_text(x=pt2, y=points[pt2], size=5, colour="blue", label="pt2", hjust=0, vjust=0)
    
    current.table <- list()
    current.table$pt1 <- time.index[pt1]
    current.table$pt1.y <- format(round(points[pt1], 3), nsmall = 3)
    current.table$pt2 <- time.index[pt2]
    current.table$pt2.y <- format(round(points[pt2], 3), nsmall = 3)
    current.table$dt <- (pt2 - pt1) * interval
    current.table$dy <- format(round((points[pt2] - points[pt1])*100, 2), nsmall = 2)
    current.table$angle <- round(atan(9/20*0.79*(points[pt2] - points[pt1]) * (last - first) / (pt2 - pt1) / (ulim - llim)) * 180 / pi)
    current.table$vol <- format(round(volatility12, 4), nsmall = 4)
    current.table <- as.data.frame(current.table)
    colnames(current.table) <- c("t1 Time", "t1 Price", "t2 Time", "t2 Price", "Move Time", 
                                 "Move BPs", "Move Angle", "Move Vol")
    
    current.table <- tableGrob(current.table)
  }
       
  #p <- p + ggtitle(paste("Prediction Time  ", time.index[current]))
  grid.newpage()
  vp1 <- viewport(width = 0.75, height = 0.95, x = 0.375, y = 0.45)
  subvp <- viewport(width = 0.3, height = 0.3, x = 0.875, y = 0.5)
  rightvp <- viewport(width = 0.3, height = 0.3, x = 0.875, y = 0.75)
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
}

library(ggplot2)

ReadParam <- function(raw) {
  a <- unlist(strsplit(raw, ","))
  a <- sapply(a, FUN = function(x) strsplit(x, "@"))
  d <- list()
  for (i in a) {
    if(length(i) < 2) {
      single <- list(x="")
    } else {
      single <- list(x=i[2])
    }
    
    names(single) <- i[1]
    d <- c(d, single)
  }
  d
}

Percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

TimeDist <- function(time1, time2) {
  abs(time1 - time2)
}

FindApproxTime <- function(time.index, start.date) {
  start.approx <- which(TimeDist(time.index, as.POSIXct(start.date)) <= 
                          as.difftime(2, units = "days"))
  if (length(start.approx) == 0) {
    stop("No approximately matched date time found in data")
  }
  start.approx[which(TimeDist(time.index[start.approx], as.POSIXct(start.date)) == 
                                min(TimeDist(time.index[start.approx], as.POSIXct(start.date))))[1]]
}

DrawSingle <- function(values, time.index, patterns, index) {
	df <- data.frame(x = 1:length(time.index), y = values)
	p <- ggplot(df, aes(x = x, y = y)) + geom_line() + 
       theme(axis.text.x = element_text(angle=90, vjust=1)) +
	     scale_y_continuous(breaks = round(seq(min(df$y), max(df$y), by = 0.02), 2)) +
	     scale_x_continuous(breaks = seq(1, length(time.index), by = 24), 
	                        labels = as.character(time.index[seq(1, length(time.index), by = 24)]))
	
	GetPlotForPattern <- function(p, df, pattern) {
		df$match <- rep("none", nrow(df))
		df[df$x == pattern$t1.x, "match"] <- "t1"	
		df[df$x == pattern$t2.x, "match"] <- "t2"
		df[df$x == pattern$t3.x, "match"] <- "t3"

		p <- p + geom_text(data = subset(df, match == "t1"), 
                       aes(x=x, y=y, size=5, colour="red", label="t1", hjust=0, vjust=0))
    
		p <- p + geom_text(data = subset(df, match == "t2"), 
                       aes(x=x, y=y, size=5, colour="blue", label="t2", hjust=0, vjust=0))
    
		p <- p + geom_text(data = subset(df, match == "t3"), 
                       aes(x=x, y=y, size=5, colour="green", label="t3", hjust=0, vjust=0))
    return(p)
	}

	row <- patterns[index, ]
	p <- GetPlotForPattern(p, df, row)
	p
}


DrawAll <- function(values, time.index, patterns) {
	df <- data.frame(x = 1:length(time.index), y = values)
	p <- ggplot(df, aes(x = x, y = y)) + geom_line() +
	     theme(axis.text.x = element_text(angle=90, vjust=1)) +
	     scale_y_continuous(breaks = round(seq(min(df$y), max(df$y), by = 0.02), 2)) +
	     scale_x_continuous(breaks = seq(1, length(time.index), by = 24), 
                        labels = as.character(time.index[seq(1, length(time.index), by = 24)]))
	
	
	GetPlotForPattern <- function (p, df, pattern) {
		df$match = rep("none", nrow(df))
		df[which(df$x == pattern$t1.x), "match"] <- "t1"  
		df[which(df$x == pattern$t2.x), "match"] <- "t2"
		df[which(df$x == pattern$t3.x), "match"] <- "t3"
				
		p <- p + geom_text(data = subset(df, match == "t1"), 
                       aes(x=x, y=y+0.005), size=5, colour="green3", label="t1")
		p <- p + geom_text(data = subset(df, match == "t2"), 
                       aes(x=x, y=y), size=5, colour="blue3", label="t2")
		p <- p + geom_text(data = subset(df, match == "t3"), 
                       aes(x=x, y=y-0.005), size=5, colour="red3", label="t3")
		return(p)
	}

	# If no pattern is identified, just output price curve
  if (length(patterns) == 0) {
    return(p)
	}
  
  range <- c(1:nrow(patterns))
	for (i in range) {
		row <- patterns[i, ]
    p <- GetPlotForPattern(p, df, row)
	}
	p
}

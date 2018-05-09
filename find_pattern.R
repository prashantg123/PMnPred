library(devtools)
load_all("d:/InfoTrie/global")

zz <- file("d:/InfoTrie/output/error.txt", open="wt")
sink(zz, type="message")

args <- commandArgs(TRUE)

input <- args[1]

params <- ReadParam(input)

#write.csv(input, paste0(params$app.path,"/output/input.csv"))

data<- read.csv(params$file.name, header = TRUE)
values <- data[, 4]
time.index <- as.POSIXct(strptime(data[, 1], '%m/%d/%Y %H:%M'))

time.nas <- which(!is.na(time.index))

# Dicard NAs
time.index <- time.index[time.nas]
values <- values[time.nas]

start.date <- params$start.date
if (start.date == "") {
  start.date <- "2013-06-24 15:15"
}

end.date <- params$end.date
if (end.date == "") {
  end.date <- "2013-06-29 02:20"
}

start <- which(time.index == as.POSIXct(start.date))
if (length(start) == 0) { # No exact match of input datetime
  start <- FindApproxTime(time.index, start.date)
}

end <- which(time.index == as.POSIXct(end.date))
if (length(end) == 0) { # No exact match of input datetime
  end <- FindApproxTime(time.index, end.date)
}

min.basis <- as.numeric(params$min.basis)
if (is.na(min.basis)) {
  min.basis <- 4
}
min.basis <- min.basis / 100

move.duration <- as.numeric(params$move.duration)
if (is.na(move.duration)) {
  move.duration <- 60
}

move.angle <- as.numeric(params$move.angle)
if (is.na(move.angle)) {
  move.angle <- 45
} 

noise.basis <- as.numeric(params$noise.basis)
if (is.na(noise.basis)) {
  noise.basis <- 2
}
noise.basis <- noise.basis / 100

noise.duration <- as.numeric(params$noise.duration)
if (is.na(noise.duration)) {
  noise.duration <- 60
}

retrace.percent <- as.numeric(params$retrace.percent)
if (is.na(retrace.percent)) {
  retrace.percent <- 30
}

retrace.min <- as.numeric(params$retrace.min)
if (is.na(retrace.min)) {
  retrace.min <- 10
}

retrace.duration <- as.numeric(params$retrace.duration)
if (is.na(retrace.duration)) {
  retrace.duration <- 60
}

vol.max <- as.numeric(params$vol.max)
if (is.na(vol.max)) {
  vol.max <- 0.001
}

width <- as.numeric(params$width)
if (is.na(width)) {
  width <- 12
}


range <- start:end
points <- values[range]
points.length <- length(points)
points.max <- max(points)
points.min <- min(points)

if (!is.na(as.numeric(params$min.basis)) && !is.na(as.numeric(params$move.duration))) { # input min.basis and move.duration
  
  patterns <- FindAll(points, interval = interval, 
                      min.basis = min.basis, move.duration = move.duration, 
                      noise.basis = noise.basis, noise.duration = noise.duration, 
                      retrace.percent = retrace.percent, 
                      retrace.min = retrace.min, retrace.duration = retrace.duration)
  p <- DrawAll(points, time.index[range], patterns) +
    ggtitle(paste("move.basis >=", min.basis*100, "bps , move.duration <", move.duration, "mins , volatility <=", vol.max, "\n", 
                       "noise.basis <", noise.basis*100, "bps , noise.duration <", noise.duration, "mins\n", 
                       "retrace.percent >=", retrace.percent, 
                       "% , retrace.min >=", retrace.min, "mins , retrace.duration <", retrace.duration, "mins, ", 
                  nrow(patterns), "patterns identified"))
  
} else if (!is.na(as.numeric(params$min.basis)) && !is.na(as.numeric(params$move.angle))) { # input min.basis and move.angle

  move.duration <- 9/20*interval * (points.length - 1) * min.basis / (points.max - points.min) / tan(move.angle*pi/180)
  
  patterns <- FindAll(points, interval = interval, 
                      min.basis = min.basis, move.duration = move.duration, 
                      noise.basis = noise.basis, noise.duration = noise.duration, 
                      retrace.percent = retrace.percent, 
                      retrace.min = retrace.min, retrace.duration = retrace.duration)
  p <- DrawAll(points, time.index[range], patterns) +
    ggtitle(paste("move.basis >=", min.basis*100, "bps , move.angle >=", move.angle, "degrees , volatility <=", vol.max, "\n", 
                       "noise.basis <", noise.basis*100, "bps , noise.duration <", noise.duration, "mins\n", 
                       "retrace.percent >=", retrace.percent, 
                       "% , retrace.min >=", retrace.min, "mins , retrace.duration <", retrace.duration, "mins, ", 
                  nrow(patterns), "patterns identified"))
  
} else if (!is.na(as.numeric(params$move.duration)) && !is.na(as.numeric(params$move.angle))) { # input move.duration and move.angle

  min.basis <- 20/9*(move.duration / interval) * (points.max - points.min) * tan(move.angle*pi/180) / (points.length - 1)
  
  patterns <- FindAll(points, interval = interval, 
                      min.basis = min.basis, move.duration = move.duration, 
                      noise.basis = noise.basis, noise.duration = noise.duration, 
                      retrace.percent = retrace.percent, 
                      retrace.min = retrace.min, retrace.duration = retrace.duration)
  p <- DrawAll(points, time.index[range], patterns) +
    ggtitle(paste("move.duration <", move.duration, "mins , move.angle >=", move.angle, "degrees , volatility <=", vol.max, "\n", 
                       "noise.basis <", noise.basis*100, "bps , noise.duration <", noise.duration, "mins\n", 
                       "retrace.percent >=", retrace.percent, 
                       "% , retrace.min >=", retrace.min, "mins , retrace.duration <", retrace.duration, "mins, ", 
                  nrow(patterns), "patterns identified"))
}


output.file <- params$output.file

if (output.file == "") {
  output.file <- "Chart.jpg"
}

# if (file.exists(output.file)) {
#   file.remove(output.file)
# }

jpeg(output.file,  width = 15, height = 9, units = "in", res = 300)

p

dev.off()

if (is.data.frame(patterns)) {
  patterns2 <- patterns
  patterns2$t1.x <- time.index[range][patterns2$t1.x]
  patterns2$t2.x <- time.index[range][patterns2$t2.x]
  patterns2$t3.x <- time.index[range][patterns2$t3.x]
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
  write.csv(NULL, paste0(params$app.path,"/output/patterns.csv"), quote=FALSE)
}

#DrawSingle(points, time.index[range], patterns, 1)
#DrawSingle(points, time.index[range], patterns, 2)

sink()
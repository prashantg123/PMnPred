library(devtools)
load_all("d:/InfoTrie/global")

zz <- file("d:/InfoTrie/output/error.txt", open="wt")
sink(zz, type="message")

args <- commandArgs(TRUE)

input <- args[1]

params <- ReadParam(input)

write.csv(input, paste0(params$app.path,"/output/input.csv"))

data<- read.csv(params$file.name, header = TRUE)
values <- data[, 4]
time.index <- as.POSIXct(strptime(data[, 1], '%m/%d/%Y %H:%M'))

time.nas <- which(!is.na(time.index))

# Dicard NAs
time.index <- time.index[time.nas]
values <- values[time.nas]

start.date <- params$start.date
if (start.date == "") {
  start.date <- "2013-01-14"
}

end.date <- params$end.date
if (end.date == "") {
  end.date <- "2014-06-14 0:50"
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
  min.basis <- 9
}
min.basis <- min.basis / 100

move.duration <- as.numeric(params$move.duration)
if (is.na(move.duration)) {
  move.duration <- 180
}

move.angle <- as.numeric(params$move.angle)
if (is.na(move.angle)) {
  move.angle <- 30
} 

noise.basis <- as.numeric(params$noise.basis)
if (is.na(noise.basis)) {
  noise.basis <- 2
}
noise.basis <- noise.basis / 100

noise.duration <- as.numeric(params$noise.duration)
if (is.na(noise.duration)) {
  noise.duration <- 120
} 

retrace.percent <- as.numeric(params$retrace.percent)
if (is.na(retrace.percent)) {
  retrace.percent <- 30
}

retrace.min <- as.numeric(params$retrace.min)
if (is.na(retrace.min)) {
  retrace.min <- 40
}

retrace.duration <- as.numeric(params$retrace.duration)
if (is.na(retrace.duration)) {
  retrace.duration <- 3000
}

vol.max <- as.numeric(params$vol.max)
if (is.na(vol.max)) {
  vol.max <- 0.01
}

width <- as.numeric(params$width)
if (is.na(width)) {
  width <- 12
}

predict.time <- params$predict.date
if (predict.time == "") {
  predict.time <- as.character(tail(time.index, 1))
}

current <- which(time.index == as.POSIXct(predict.time))
if (length(current) == 0) { # No exact match of input datetime
  current <- FindApproxTime(time.index, predict.time)
}

# if (current - start < 60/interval * 24 * 125 + 168) {
#   stop("Not enough training data. Use later time")
# }


range <- start:current
points <- values[range]
points.length <- length(points)
points.max <- max(points)
points.min <- min(points)

if (!is.na(as.numeric(params$min.basis)) && !is.na(as.numeric(params$move.angle))) { # input min.basis and move.angle

  move.duration <- 9/20*interval * (points.length - 1) * min.basis / (points.max - points.min) / tan(move.angle*pi/180) 
    
} else if (!is.na(as.numeric(params$move.duration)) && !is.na(as.numeric(params$move.angle))) { # input move.duration and move.angle

  min.basis <- 20/9*(move.duration / interval) * (points.max - points.min) * tan(move.angle*pi/180) / (points.length - 1)

}

patterns <- FindAll(points, interval = interval, 
                    min.basis = min.basis, move.duration = move.duration, 
                    noise.basis = noise.basis, noise.duration = noise.duration, 
                    retrace.percent = retrace.percent, 
                    retrace.min = retrace.min, retrace.duration = retrace.duration)

period <- 60

current <- current - start + 1

if (exists("HFdata")) {
  HFdata <- Delta(HFdata, points, current, patterns, interval, period)
} else { 
  HFdata <- Preprocess(points, current, patterns, interval, period)
}

output.file <- params$output.file

if (output.file == "") {
  output.file <- "Chart.jpg"
}

trend <- tolower(params$trend)
if (trend == "") {
  predict.time <- "up"
}

jpeg(output.file,  width = 20, height = 9, units = "in", res = 300)

Predict(HFdata, current, time.index[range])

dev.off()

sink()
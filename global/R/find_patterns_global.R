# Function to find all t1,t2,t3 points for given values
FindAll <- function(points, interval, 
                    min.basis, move.duration,
                    noise.basis, noise.duration, 
                    retrace.percent, retrace.min, retrace.duration) {  
  points.length <- length(points)
  
	# data frame to save the patterns
	patterns <- NULL
  
	# traverse through all values of the vector
	i <- 1
	while (i < points.length) {
		local.points <- points[i:points.length]
		pattern <- FindSingle(local.points, interval, (i - 1), 
		                      min.basis = min.basis, move.duration = move.duration, 
		                      noise.basis = noise.basis, noise.duration = noise.duration, 
		                      retrace.percent = retrace.percent, 
		                      retrace.min = retrace.min, retrace.duration = retrace.duration)

		if (is.data.frame(pattern)) {
			if (nrow(pattern) == 1 
          && (pattern$t2.x - pattern$t1.x) > 2) {  # filter jumps with small gap (<=10mins)
        
			  pattern["t12"] <- pattern$t2.x - pattern$t1.x
			  pattern["t23"] <- pattern$t3.x - pattern$t2.x

			  # Filter by volatility
        vol12 <- sd(points[pattern$t1.x : pattern$t2.x]) / pattern$t12 * 100
			  vol23 <- sd(points[pattern$t2.x : pattern$t3.x]) / pattern$t23 * 100
			  
			  if (vol12 <= vol.max) {
			    pattern["vol12"] <- vol12
			    pattern["vol23"] <- vol23
          
			    # When we find a pattern, we increment the index to the next t2 position 
			    i <- pattern$t2.x - 1
			    
			    if (is.data.frame(patterns)) {
			      patterns <- rbind(patterns, pattern)
			    } else {
			      patterns <- pattern
			    }
			  }
        
			}
		}
    
		i <- i + 1
	}
	
  if(length(patterns) == 0) {
    return(patterns)
  }
  
	patterns$dy21 <- patterns$t2.y - patterns$t1.y
	patterns$dy32 <- patterns$t3.y - patterns$t2.y
	patterns$retrace.pct <- -patterns$dy32 / patterns$dy21
  
  return(patterns)
}


FindSingle <- function(points, interval, x.offset = 0,
                       min.basis, move.duration,  
                       noise.basis, noise.duration, 
                       retrace.percent, 
                       retrace.min, retrace.duration) { 
	# Finds the t2 and t3 point for a given t1 and data values.
  #
  # Args:
  #   points: Price vector
  #
  # Returns:
  #   Pattern of t1, t2, t3 for starting point of "points" or null if none
  points.length <- length(points)
  move.duration <- move.duration %/% interval
  noise.duration <- noise.duration %/% interval
  retrace.min <- retrace.min %/% interval
  retrace.duration <- retrace.duration %/% interval
  
  i <- 2
  
  # Search from next point for move larger than min.basis.
  while (abs(points[i] - points[1]) < min.basis 
         && (i - 1) < move.duration) {
    i <- i + 1
    if (i > points.length) {
      return(NULL)
    }
  }
  
  # If there is no large move within move.duration, return null.
  if (i - 1 >= move.duration) {
    return(NULL)
  }
	
  # Otherwise i is the first point that is beyond min.basis, which could be t2.
  # Then we try to determine whether it is a t2.
  
  # If price still moves in the same direction, continue to next point.
  if ((i+1) > points.length) {
    return(NULL)
  }
  
  while ((points[i+1] - points[i])*(points[i] - points[1]) >= 0 
         && (i+1) - 1 < move.duration) {
    i <- i + 1
    if ((i+1) > points.length) {
      return(NULL)
    }
  }
  
  # If large move doesn't stop within move.duration, return null.
  if ((points[i+1] - points[i])*(points[i] - points[1]) >= 0 
      && (i+1) - 1 >= move.duration) {
    return(NULL)
  }
  
  # Otherwise price starts to retrace in the next point. 
  # Check if it is just noise.
  j <- i + 1
  k <- i
  max.move <- 0
  while (abs(points[j] - points[i]) < noise.basis  
         && (j - i) < noise.duration) {
    if ((points[j] - points[i])*(points[i] - points[1]) > 0
        && abs(points[j] - points[i]) > max.move) {
      k <- j
      max.move <- abs(points[j] - points[i])
    }
    j <- j + 1
    if (j > points.length) {
      return(NULL)
    }
  }
  
  # If small move doesn't stop within noise.duration, return null.
  if (abs(points[j] - points[i]) < noise.basis  
      && j - i >= noise.duration) {
    return(NULL)
  }
  
  # Otherwise if price continues to move in the same direction after noise
  while ((points[j] - points[i])*(points[i] - points[1]) >= 0
      && (j - 1) < move.duration) {
    # Consider it as continuous move.
    i <- j
    
    # Repeat the whole process from line 80 to 121.
    # If price still moves in the same direction, continue to next point.
    if ((i+1) > points.length) {
      return(NULL)
    }
    
    while ((points[i+1] - points[i])*(points[i] - points[1]) >= 0 
           && (i+1) - 1 < move.duration) {
      i <- i + 1
      if ((i+1) > points.length) {
        return(NULL)
      }
    }
    
    # If large move doesn't stop within move.duration, return null.
    if ((points[i+1] - points[i])*(points[i] - points[1]) >= 0 
        && (i+1) - 1 >= move.duration) {
      return(NULL)
    }
    
    # Otherwise price starts to retrace in the next point. 
    # Check if it is just noise.
    j <- i + 1
    k <- i
    max.move <- 0
    while (abs(points[j] - points[i]) < noise.basis  
           && (j - i) < noise.duration) {
      if ((points[j] - points[i])*(points[i] - points[1]) > 0
          && abs(points[j] - points[i]) > max.move) {
        k <- j
        max.move <- abs(points[j] - points[i])
      }
      j <- j + 1
      if (j > points.length) {
        return(NULL)
      }
    }
    
    # If small move doesn't stop within noise.duration, return null.
    if (abs(points[j] - points[i]) < noise.basis  
        && j - i >= noise.duration) {
      return(NULL)
    }
  }
  
  # If price continues to move in the same direction but out of move.duration, 
  # return null.
  if ((points[j] - points[i])*(points[i] - points[1]) >= 0
      && (j - 1) >= move.duration) {
    return(NULL)
  }
  
  
  # Or price starts to retrace and move out of noise range, record extremum point as t2. 
  # Check if it will retrace to a t3.
  i <- k
  
  # If extremum point is out of move.duration, return null.
  if ((i - 1) >= move.duration) {
    return(NULL)
  }
  
  # If between t1 and i there is move that is in opposite direction to point i,
  # return null.
  for (l in 2:(i-1)) {
    if ((points[l] - points[1])*(points[i] - points[1]) < 0)
      return(NULL)
  }
  
  # Search from next point at least retrace.min away
  # for retracement larger than retrace.percent.
  j <- i + retrace.min
  if (j > points.length) {
    return(NULL)
  }
  
  while (abs(points[j] - points[i]) < retrace.percent*0.01*abs(points[i] - points[1]) 
         && (j - i) < retrace.duration
         && (points[j] - points[i])*(points[i] - points[1]) < 0) {
    j <- j + 1
    if (j > points.length) {
      return(NULL)
    }
  }
  
  # If price moves back again or out of retrace duration, return null.
  if ((j - i) >= retrace.duration 
      || (points[j] - points[i])*(points[i] - points[1]) >= 0) {
    return(NULL)
  }
  
  # Otherwise j is the first point that is beyond retrace.percent, which could be t3.
  # Then we try to determine whether it is a t3.
  
  # If price still retraces in the same direction, continue to next point.
  if ((j+1) > points.length) {
    return(NULL)
  }
  
  while ((points[j+1] - points[j])*(points[j] - points[i]) >= 0 
         && (j+1) - i < retrace.duration) {
    j <- j + 1
    if ((j+1) > points.length) {
      return(NULL)
    }
  }
  
  # If large retrace doesn't stop within retrace.duration, return null.
  if ((points[j+1] - points[j])*(points[j] - points[i]) >= 0 
      && (j+1) - i >= retrace.duration) {
    return(NULL)
  }
  
  # Otherwise price starts to move back in the next point. 
  # Check if it is just noise (similiar to checking for t2, replace j->l i->j 1->i).
  l <- j + 1
  k <- j
  max.move <- 0
  while (abs(points[l] - points[j]) < noise.basis  
         && (l - j) < noise.duration) {
    if ((points[l] - points[j])*(points[j] - points[i]) > 0
        && abs(points[l] - points[j]) > max.move) {
      k <- l
      max.move <- abs(points[l] - points[j])
    }
    l <- l + 1
    if (l > points.length) {
      return(NULL)
    }
  }
  
  # If small move doesn't stop within noise.duration, return i and k as t2 t3.
  if (abs(points[l] - points[j]) < noise.basis  
      && l - j >= noise.duration) {
    pattern <- data.frame(t1.x = 1 + x.offset, t1.y = points[1], 
                          t2.x = i + x.offset, t2.y = points[i], 
                          t3.x = k + x.offset, t3.y = points[k])
    return(pattern)
  }
  
  # Otherwise if price continues to retrace in the same direction after noise
  while ((points[l] - points[j])*(points[j] - points[i]) >= 0
         && (l - i) < retrace.duration) {
    # Consider it as continuous move.
    j <- l
    
    # Repeat the whole process from line 216 to 263.
    # If price still retraces in the same direction, continue to next point.
    if ((j+1) > points.length) {
      return(NULL)
    }
    
    while ((points[j+1] - points[j])*(points[j] - points[i]) >= 0 
           && (j+1) - i < retrace.duration) {
      j <- j + 1
      if ((j+1) > points.length) {
        return(NULL)
      }
    }
    
    # If large retrace doesn't stop within retrace.duration, return null.
    if ((points[j+1] - points[j])*(points[j] - points[i]) >= 0 
        && (j+1) - i >= retrace.duration) {
      return(NULL)
    }
    
    # Otherwise price starts to move back in the next point. 
    # Check if it is just noise (similiar to checking for t2, replace j->l i->j 1->i).
    l <- j + 1
    k <- j
    max.move <- 0
    while (abs(points[l] - points[j]) < noise.basis  
           && (l - j) < noise.duration) {
      if ((points[l] - points[j])*(points[j] - points[i]) > 0
          && abs(points[l] - points[j]) > max.move) {
        k <- l
        max.move <- abs(points[l] - points[j])
      }
      l <- l + 1
      if (l > points.length) {
        return(NULL)
      }
    }
    
    # If small move doesn't stop within noise.duration, return i and k as t2 t3.
    if (abs(points[l] - points[j]) < noise.basis  
        && l - j >= noise.duration) {
      pattern <- data.frame(t1.x = 1 + x.offset, t1.y = points[1], 
                            t2.x = i + x.offset, t2.y = points[i], 
                            t3.x = k + x.offset, t3.y = points[k])
      return(pattern)
    }
  }
  
  # If price continues to retrace in the same direction but out of retrace.duration, 
  # return null.
  if ((points[l] - points[j])*(points[j] - points[i]) >= 0
      && (l - i) >= retrace.duration) {
    return(NULL)
  }
  
  # Or price starts to move back and out of noise range.
  # Return i and j as t2 t3.
  j <- k
  pattern <- data.frame(t1.x = 1 + x.offset, t1.y = points[1], 
                        t2.x = i + x.offset, t2.y = points[i], 
                        t3.x = j + x.offset, t3.y = points[j])
  return(pattern)
}
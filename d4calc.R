# d4calc.R - Symmetry group of the square, also known as the 'D4' group

# Create dataframe of symmetrical transformations of the square
D4.makeDataFrame <- function() {
  DF <- data.frame(name=rep("", 8), 
                   vertexConfiguration = rep("", 8), 
                   stringsAsFactors=FALSE)
  
  DF[1,] <- c("i", "ABCD")
  DF[2,] <- c("r", "DABC")
  DF[3,] <- c("s", "CDAB")
  DF[4,] <- c("t", "BCDA")
  DF[5,] <- c("w", "BADC")
  DF[6,] <- c("x", "DCBA")
  DF[7,] <- c("y", "ADCB")
  DF[8,] <- c("z", "CBAD")
  
  return(DF)
}

DF <- D4.makeDataFrame()

# Show the 8 possible configurations of labeled vertices of square
D4.showConfigs <- function(DF) {
  par(mar = c(1, 1, 1, 1))
  
  plot(NULL, 
       xlim = c(0, 30), 
       ylim = c(-1, 4), 
       asp = 1, 
       axes = FALSE)
  
  for (i in 0:7) {
    points(c(0, 2, 2, 0, 0) + 4 * i, 
           c(0, 0, 2, 2, 0), 
           type = "l")
    
    label <- strsplit(DF[i + 1, 2], "")[[1]]
    
    text(c(0.2, 1.8, 1.8, 0.2) + 4 * i, 
         c(1.8, 1.8, 0.2, 0.2), 
         label)
    
    text(1 + 4 * i, 
         -0.5, 
         DF[i + 1, 1])
    
    segments(c(17, 19.5, 23.5, 30.5), 
             c(-0.25, 1, 2.5, 2.5),
             c(17, 22.5, 26.5, 27.5),
             c(2.5, 1, -0.5, -0.5),
             lty = 2)
  }
}

D4.showConfigs(DF)

# 'vertexConfiguration' is string of vertex lables, 
  # to be placed around the square clockwise from top left
D4.showSquare <- function(vertexConfiguration){
  par(mar = c(1, 1, 1, 1))
  
  plot(NULL, 
       xlim = c(0, 4), 
       ylim = c(-1, 2), 
       asp = 1, 
       axes = FALSE)
  
  points(c(0, 2, 2, 0, 0), 
         c(0, 0, 2, 2, 0), 
         type = "l", 
         lwd = 2)
  
  label <- strsplit(vertexConfiguration, "")[[1]]
  
  text(c(0.2, 1.8, 1.8, 0.2), 
       c(1.8, 1.8, 0.2, 0.2), 
       label)
}

D4.showSquare("ABCD")

# Apply symmetrical transformation operation to current vertex config
 D4.apply <- function(operation, vertexConfiguration){
  v <-strsplit(vertexConfiguration, "")[[1]]
  w <- switch(operation,
              "i" = v,
              "r" = c(v[4], v[1], v[2], v[3]),
              "s" = c(v[3], v[4], v[1], v[2]),
              "t" = c(v[2], v[3], v[4], v[1]),
              "w" = c(v[2], v[1], v[4], v[3]),
              "x" = c(v[4], v[3], v[2], v[1]),
              "y" = c(v[1], v[4], v[3], v[2]),
              "z" = c(v[3], v[2], v[1], v[4])
  )
  newVertexConfig <- paste(w, sep="", collapse="") 
  return(newVertexConfig)
}

# Multiply two symmetry transformation operations 'a' and 'b'
D4.multiply <- function(DF, a, b){
  # Look up the name of operation 'b'
  index <- which(DF$name == b)[1]
  
  # Find corresponding configuration of vertices
  vertexConfiguration <- DF$vertexConfiguration[index]
  
  # Apply group operation 'a' to the particular vertex configuration
  newVertexConfig <- D4.apply(a, vertexConfiguration)
  
  # Look up resulting vertex configuration
  index <- which(DF$vertexConfiguration == newVertexConfig)[1]
  return (DF$name[index])
}

vD4.multiply <- Vectorize(D4.multiply, c("a", "b"))


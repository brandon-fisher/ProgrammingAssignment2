# makeCacheMatrix takes a matrix, inverts it and stores the inversion in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {   #this stores into cache using <<-
    x <<- y
    m <<- NULL
  }
  get <- function() x   #this takes the function entered
  setinverse <- function(inverse) m <<- inverse   #sets the inverted function defined later
  getinverse <- function() m  #calls the inverted matrix defined later
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve takes the cache from makeCacheMatrix and inverts the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()   #calls the getinverse function from makeCacheMatrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()  #calls the get function from makeCacheMatrix
  m <- solve(data, ...)   #inverts the matrix
  x$setinverse(m)   #calls the setinverse function from makeCacheMatrix
  m  #prints the inverted matrix
}

# Here is sample usage
# test_matrix <- matrix(1:4,2,2)
# test_MCM <- makeCacheMatrix(test_matrix)
# cacheSolve(test_MCM)
# the result is
#
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
#
# Here is another example
# test_matrix2 <- matrix(
#   runif(1:16),
#   nrow = 4
#  )
#
# testMCM2 <- makeCacheMatrix(test_matrix2)
# cacheSolve(testMCM2)
#
# the result is 
#
# [,1]      [,2]      [,3]       [,4]
# [1,]  2.2597354 -2.739292  1.787617 -0.7258198
# [2,] -2.1530279 -1.837578  1.407458  2.9681336
# [3,]  0.3291513  1.104407 -1.136810  0.2686555
# [4,] -1.2706393  3.975657 -1.555460 -0.9676047

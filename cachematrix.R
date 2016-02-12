#This script contains two functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix essentially returns a list to set or get a matrix and to set or get the inverse of the matrix
##For instance, try running:
# a <- makeCacheMatrix( matrix(c(1,2,12,13), nrow = 2, ncol = 2) );
# a$get();
#

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that returns the inverse of a matrix x
#Prior to computing the inverse, the function tries to pull the inverse from makeCacheMatrix.
# If this is null, i.e. the inverse has not been computed yet, it will compute the inverse.
# Otherwise, there is no need to recalculate and the results will be pulled using the function getInverse()


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

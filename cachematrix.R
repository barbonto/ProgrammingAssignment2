## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverted <- function(inverted) i <<- inverted
  getinverted <- function() i
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverted()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getinverted()
  i <- solve(data, ...)
  x$setinverted(i)
  i
}


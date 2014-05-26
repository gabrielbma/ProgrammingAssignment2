## This file contains functions responsible for the computation of the 
## inverse of a special square "matrix" and caching its results to improve 
## computation speed in subsequent calls.

## This function wraps a regular R square matrix in order to add 
## cache capabilities. The new wrapped matrix contains methods to
## set a matrix, to get the original matrix, to get the inverse matrix
## and to set the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function is responsible for the computation of inverse of a matrix 
## retrieved from makeCacheMatrix() and caching the result so that subsequent 
## calls to this function returns the result already computed previously.
## This function calls the R solve function to compute the inverse of the 
## matrix. It also assumes that the supplied matrix is always invertible
## and is square. 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

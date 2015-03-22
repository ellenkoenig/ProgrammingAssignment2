## The code in this file provides functions that compute the inverse of 
## an invertible matrix using caching

## This function converts a normal R matrix into a matrix with an 
## attached cache for its inverse. If no matrix is provided as 
## an argument, an empty matrix will be used
##
## The return object provides the following methods
## get() to fetch the original matrix
## getinverse() to fetch the inverse
## set() to set the original value
## setinverse() to set the inverse

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


## Computes the inverse of the matrix object returned by makeCacheMatrix
## Will use cached data if available to speed up the computation
## Input: matrix, must be square, invertible and in the format provided by makeCacheMatrix
## Output: Inverted matrix
## Will also set the cache of the provided matrix object with the inverse for future reference
cacheSolve <- function(x, ...) {
  i = x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

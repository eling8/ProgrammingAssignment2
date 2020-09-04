## This set of functions implements a matrix with cached inverse,
## allowing the value of the inverse to be looked up in the cache
## rather than being computed repeatedly.

## This function creates a special matrix with the ability to
## get/set the value of the matrix, as well as get/set the value
## of the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function (newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Given a cache matrix created using `makeCacheMatrix`, returns
## the inverse of the matrix. If the inverse is available in the
## cache, returns this cached value; else calculates the inverse and
## caches it for future use.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Returning cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  return(inverse)
}

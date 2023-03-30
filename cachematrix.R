## The first of two functions. Creates a matrix object that can cache its own inverse. 
## This follows the same pattern as the "makeVector" and "cachemean" functions provided as
## examples by the course.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }

get <- function() x
setInverse <- function(inverse) matrix_inv <<- inverse
getInverse <- function() matrix_inv
list (set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)

}


## The second of two functions. Calculates the inverse of the matrix object created by
## the "makeCacheMatrix" function above. If the inverse has already been calculated, and the 
## matrix has not been changed, then the function instead retrieves the inverse from the
## cache.

cacheSolve <- function(x, ...) {

  ## If the inverse has already been calculated, retrieve inverse from cache and display.
  inv <- x$getInverse()
  if (!is.null(matrix_inv)) {
    message("Getting Cached Data")
    return(matrix_inv)
  }
  ## If the inverse has not been calculated, calculate and cache the inverse.
  mat <- x$get()
  matrix_inv <- solve(mat, ...)
  x$setInverse(matrix_inv)
  matrix_inv
}

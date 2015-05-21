## These functions provide the functionality to cache the result
## of a matrix inversion, since this can be a costly computation
## and we might need to access the same result more than once
## in a given application. The result of the matrix inversion is
## cashed in a separate R environment by using the <<- operator.

## Creates a matrix with the feature of being 
## able to cache its inverse value (technically it's a list)

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- null
  }
  get <- function() x
  setinv <- function(invrs) xinv <<- invrs
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of the cacheable matrix x, but checks
## before computing if the result is already in memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}

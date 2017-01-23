## This function calculates the inverse of a matrix,
## makes a set function that can store it,
## caches the value in the setinv function,
## defines a getter functions get and getinv,
## and makes a list of the functions that can be used by cachesolve.

##
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## 

cacheSolve <- function(x, ...) {
  ## If the inverse was not yet calculated, 
  ## it returns a matrix that is the inverse of 'x'.
  ## If the inverse was calculated before, 
  ## it returns a massage, and the matrix.
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

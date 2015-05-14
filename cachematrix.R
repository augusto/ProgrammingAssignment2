## Caches an matrix that is expensive to compute (memoization)
## the Object has the following 4 methods
## get() gets the matrix
## set(m) sets a matrix m. This operation clears any previously cached matrix
## getinv() gets the cached inversed matrix
## setinv(m) sets (caches) the inversed matrix m 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Calculates the inverse of a matrix and caches the result. Receives the same
## parameters as the internal function solve(). Run ?solve for more information
##
## Returns an errror if the matrix cannot be inverted (https://en.wikipedia.org/wiki/Invertible_matrix)
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}

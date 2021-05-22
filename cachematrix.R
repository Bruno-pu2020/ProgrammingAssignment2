## Coursea Assignment 2 - Matrix Inversion Cache
## Pair of functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse
## Outputs functions to be called on if needed to calc inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set, get = get,
    setinvert = setinvert,
    getinvert = getinvert)
}


## Checks if inverse already cached, if so returns, if not calculates

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}

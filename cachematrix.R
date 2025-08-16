## This function helps to remember the results of matrix inverse and serves it when you ask
##instead of recalculating every time

## Builds an object that can store a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This checks if the inverse is already stored it spits the data without 
##recalculating. If not it calculates the inverse, stores it and gives the data.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinv(inv)
  inv
}

## These series of functions will be submitted in Coursera as partial fulfillment in the R Programming.

## This function will create a special matrix obejct that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will compute the inverse of the special matrix returned by makeCacheMatrix at the top.
## The cachesolve should retrieve the inverse from the cache if the inverse was already calculated (and the matrix was not changed)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

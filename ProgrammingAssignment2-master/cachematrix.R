## This function will solve the inverse of a matrix and cache it.

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

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
   
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

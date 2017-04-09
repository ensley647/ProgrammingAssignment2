## These functions enable creation of the inverse of a matrix
## and they cache the inverse for future use.

## makeCacheMatrix accepts a square matrix as input and 
## creates the setter and getter methods for the matrix
## as well as the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve either returns the cached inverse of a matrix
## or creates and returns the inverse if it is not already cached

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


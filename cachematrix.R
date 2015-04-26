## These functions are aimed to compute the inverse of a matrix and store the result to be used in future computations.

## makeCacheMatrix creates a list of functions that store a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function(){ inv  }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

## This function compute the inverse of a matrix if it is not already stored in the cache
  
  cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }


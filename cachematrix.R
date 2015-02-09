
### Pair of functions that cache the inverse of a matrix.

## CacheMatrix constructor function.
## CacheMatrix is a wrapper that wraps an invertible square matrix and is able to cache the inverse of the square matrix 
## Param x is an invertible square matrix
## Returns a list that contains the following methods:
## set: set the wrapped invertible square matrix
## get: get the wrapped invertible square matrix
## setinv: stores the inverse of the wrapped invertible square matrix
## getinv: retrieves the inverse of the wrapped invertible square matrix
makeCacheMatrix <- function(x = matrix()) {
  inv.x <- NULL
  
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) inv.x <<- inv
  getinv <- function() inv.x
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve takes a cachematrix as argument.
## If the inverse is already computed and cached in the cacheMatrix it returns it.
## Otherwise computes the inverse, stores it in the cacheMatrix and returns it
## Param x is a cacheMatrix (created with the makeCacheMatrix function)
cacheSolve <- function(x, ...) {
  result <- x$getinv()
  
  if (is.null(result)) {
    data <- x$get()
    result <- solve(data, ...)
    x$setinv(result)    
  } else {
    message("getting cached data")
  }
  
  result
}

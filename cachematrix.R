## These functions can cache the inverse of a matrix, so you do not need to
## re-calculate the inverse if you need it many times.

## Creates an object, that can cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
    
  get <- function() {
    x
  }
  
  setinverted <- function(inverted) {
    i <<- inverted
  }
  
  getinverted <- function() {
    i
  }
  
  ## Creates and returns list consisting of the four functions
  
  list(set = set, get = get, setinverted = setinverted,
       getinverted = getinverted)

}

## Computes the inverse of the matrix returned by makeCacheMatrix.
## If it has already been calculated (and the matrix is unchanged), 
## then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverted()

  ## Checks if the inverted matrix is already cached, if yes, it returns it

  if(!is.null(i)) {
    message("getting cached matrix")
    return(i)
  }
  
  ## If inverted matrix is not cached it creates it with solve(), caches it 
  ## with setinverted and returns it
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverted(i)
  i
  
}
  
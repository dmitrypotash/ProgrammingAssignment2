## Put comments here that give an overall description of what your
## functions do

## Creates a closure which holds the cache and provides the functions to work with it.


makeCacheMatrix <- function(x = matrix()) {
  
  ## Cache value
  inverse_value <- NULL;
  
  ## function to set a new matrix value
  s <- function(new_matrix) { 
    x <<- new_matrix
    inverse_value <- NULL
  }
  
  ## generating wrappers for the cached matrix object
  list(set = s,
       get = function() x,
       setinverse = function(inverse) inverse_value <<- inverse,
       getinverse = function() inverse_value)
}


## Finds the inverse of the matrix and caches the value.
## Uses the functions provided by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  ## Trying to get the cached inverse value
  i <- x$getinverse()
  
  
  if (is.null(i)) {
    
    ## If value is not set, computing the inverse value
    m <- x$get()
    message("Computing inverse matrix")
    
    i <- solve(m, ...)
  
    ## Saving the inverse value in cache
    x$setinverse(i)
  }
  else {
    
    ## Writing out the message that the value was taken from cache
    message("Getting inverse matrix from cache.")
  }
  
  ## Return value
  i
}

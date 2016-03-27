## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_value <- NULL;
  s <- function(new_matrix) { 
    x <<- new_matrix
    inverse_value <- NULL
  }
  list(set = s,
       get = function() x,
       setinverse = function(inverse) inverse_value <<- inverse,
       getinverse = function() inverse_value)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (is.null(i)) {
    m <- x$get()
    print("Computing inverse matrix")
    i <- solve(m, ...)
    x$setinverse(i)
  }
  else {
    print("Getting inverse matrix from cache.")
  }
  i
}

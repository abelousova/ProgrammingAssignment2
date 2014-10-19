## This file contains functions for caching inverse of matrix.

## This function creates a special "matrix" object that can cache its inverse.
## Original matrix is passed through argument 'x', default argument is empty matrix
## Cached inverse of matrix is stored in variable 'solve', initial value is null
makeCacheMatrix <- function(x = matrix()) {
  solve <- NULL
  
  ## Updates value of original matrix and sets cached inverse to null 
  ## (we mustn't use old inverse for the new value of original matrix)
  set <- function(y) {
    x <<- y
    solve <<- NULL
  }
  
  ## Returns original matrix
  get <- function() x
  
  ## Sets argument's value to inverse
  setsolve <- function(solvem) solve <<- solvem
  
  
  ## Returns cached inverse of matrix
  getsolve <- function() solve
  
  ## Return list of "matrix" object's methods
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  
  ## if the value of inverse has already been calculated
  if(!is.null(s)) { 
    message("getting cached data")
    return(s)
  }
  
  ## if the value of inverse is NOT calculated
  data <- x$get() ## get the value of original matrix
  s <- solve(data, ...) ## calculate inverse of matrix
  x$setsolve(s) ## store inverse of matrix to cache
  s ## return inverse of matrix
}
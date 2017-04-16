## Assignment: Caching the Inverse of a Matrix
#Matrix inversion is usually a costly computation
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a matrix object that can cache its inverse
#It must return a list of functions to:
#Set the matrix, get the matrix, set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Function to set the matrix to a new matrix and the inverse to null
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  #Function to return the current matrix
  get <- function() x
  #Function to set the inverse
  setInverse <- function(inverse) inv <<-inverse
  #Function to return the currently cached inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #Get the currently cached inverse value
  inv <- x$getInverse()
  #Check to see if the cached inverse is null, if not return it
  if(!is.null(inv)) {
    message("getting inverse")
    return(inv)
  }
  #If the cached inverse is null, get the matrix and calculate the inverse
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}



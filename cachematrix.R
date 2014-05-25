#Coursera: R Programming Assignment2:
#Your assignment is to write a pair of functions that cache the inverse of a matrix.
#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#For this assignment, assume that the matrix supplied is always invertible.


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize
  m <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setinverse<-function(solve) m<<- solve
  
  ## Method to get the inverse of the matrix
  getinverse<-function() m
  
  
  ## Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("Retrieving cached data")
    return(m)
  }
  
  ## Get the matrix
  mx <- x$get()
  
  ## Calculate the inverse
  m <- solve(mx, ...)
  
  ## Set the inverse to the object
  x$setinverse(m)
  
  ## Return the matrix
  m
}

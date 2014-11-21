# Programming Assignment 2 - Caching the Inverse of a Matrix
# Write a pair of functions that cache the inverse of a matrix

# This function creates a special "matrix" object that can cache its inverse.
# First, it sets the inverse property, then sets the matrix, 
# and then prints the matrix. 
# Then the inverse matrix is setted and printed.
# At the end, list of methods are listed

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() {
    m
  }
  
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- m$getinverse()
  
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data)
  m$setinverse(i)
  
  i
}
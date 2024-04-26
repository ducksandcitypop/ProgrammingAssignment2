## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.

## By modifying the previous example, the function
## creates a list of functions which encodes 
## information about the inverse of a matrix.
## setinverse computes the inverse.
## getinverse pushes back the inverse after computation.
## set initializes matrix computation.
## get retrieves the original matrix.

makeCacheMatrix <- function(z = matrix()) {
  m <- NULL
  set <- function(y) {
    z <<- y
    m <<- NULL
  }
  get <- function() z
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function.

## cacheSolve begins by trying to retrieve the inverse
## of the desired matrix. If the cache is present,
## it returns the inverse matrix.
## If not, it computes it using the list of functions
## created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Retrieving inverse matrix...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
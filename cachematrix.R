## Matrix Caching Functions for Coursera R Programming - Assignment 2
## Adapted from Vector example

## makeCacheMatrix:
## This function creates a matrix object that can have its inverse cached.

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setInv <- function(inv) v <<- inv
  getInv <- function() v
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve:
## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves
## inverse from the cache.

cacheSolve <- function(x = matrix()) {
  v <- x$getInv()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  v <- solve(x$get())
  x$setInv(v)
  v
}

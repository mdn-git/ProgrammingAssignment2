##################################################################
## cachematrix.R : Functions to cache the inverse of a matrix   ##   
##     The matrix supplied must always be invertible            ##
##################################################################

## How-to use:
##
## source("cachematrix.R")
## matx <- matrix(c(2,2,3,2), nrow = 2, ncol=2)
## cached_matx <- makeCacheMatrix(matx)
## cachesolve(cached_matx)
##
##
## 1st execution of cachesolve (no cache on the first execution) 
## > cacheSolve(cached_matx)
##     [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0
## 2nd execution of cachesolve(cached)
##> cacheSolve(cached_matx)
##getting cached data
##      [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0



## makeCacheMatrix creates a special "matrix" containing functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
  
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve calculates the inverse of a special "matrix"
# Check if the inverse has already been calculated. If so, gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

  
}

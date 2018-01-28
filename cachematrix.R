## This functions shows special martix object which helps to caches its inverse.
##makeCacheMatrix is a special matrix and cachesolve is an inverse of matrix.

## It has a list of four functions to set and get the value of the
## matrix and inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
	setinverse <- function(inverse) inverse <<- inverse
	getinverse <- function() inverse
	list(set = set, get = get, 
	setinverse = setinverse,
	getinverse = getinverse)
 }


## Function computes the inverse of the special "matrix" which is created by 
## makeCacheMatrix from above. If the inverse which has been already calculated 
## then it will  retrieve the inverse from the cache.

 cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
	 inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting Cached Data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
 } 




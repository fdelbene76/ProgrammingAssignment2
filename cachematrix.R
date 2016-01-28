# Two functions that are used to create a special object that stores a matrix 
# and caches its inverse. It allows to repeatably solve the inverse of the matrix, but only
# calculating the inverse once.
# Usage:
# m <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# cacheSolve(m)

# This function creates a special "matrix" object, which is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		  set <- function(y) {
			x <<- y
			m <<- NULL
		  }
		  get <- function() x
		  setinv <- function(inv) m <<- inv
		  getinv <- function() m
		  list(set = set, get = get,
			   setinv = setinv,
			   getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by 'makeCacheMatrix'.
# If the inverse has already been calculated (and the matrix has not changed), then the 
# 'cachesolve' should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
		  if(!is.null(m)) {
			message("getting cached data")
			return(m)
		  }
		  data <- x$get()
		  m <- solve(data, ...)
		  x$setinv(m)
		  m
		
}

## Pair of functions that cache the inverse of a matrix

## This function, makeCacheMatrix creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function (x= matrix()) {
	m <- NULL
	setmatrix <- function(y) {
		x <<- y
		m <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list (setmatrix = setmatrix, getmatrix = getmatrix,
		  setinverse = setinverse,
		  getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrix <- x$getmatrix()
	m <- solve(matrix, ...)
	x$setinverse(m)
	m
}
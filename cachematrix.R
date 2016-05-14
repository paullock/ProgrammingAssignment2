## R Programming Assignment 2 - Caching the Inverse of a Matrix:
## The 'makeCacheMatrix' and 'cacheSolve' functions below store a matrix and then caches its inverse respectively.  

## The 'makeCacheMatrix' function creates a 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL	
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## The 'cacheSolve' function computes the inverse of the matrix created above, if this computation has already taken place then this function retrieves the result from the cashe.  For lareg computations this can save time.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	
	return(inv)
	
}

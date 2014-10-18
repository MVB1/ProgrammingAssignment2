# These functions, makeCacheMatrix () and cacheSolve (), are able to cache the inverse of a matrix
# They allow retrieval of values instead of repeated computing of the inverse (i.e. if the matrix did not change)
# They provide an excellent example how to cache potentially time-consuming computations in R

# The makeCacheMatrix function creates a special "matrix" that can cache its inverse
# Importantly, makeCacheMatrix() does not compute the inverse of the special "matrix"
# It basically generates a list and contains functions that 1) set the matrix (set), 2) get the matrix (get), 
# 3) set the cached inverse values (setSolveMatrix), and 4) get the cached inverse values (setSolveMatrix)

makeCacheMatrix <- function(x = matrix()) {
	sm <- NULL
	set <- function(y) {
		x <<- y
		sm <<- NULL
	}
	get <- function() x
	setSolveMatrix <- function(solve) 
		sm <<- solve
	getSolveMatrix <- function() sm
	list(set = set, get = get, setSolveMatrix = setSolveMatrix, getSolveMatrix = getSolveMatrix)
}

# The cacheSolve function is able to compute the inverse of the special "matrix" returned by makeCacheMatrix ()
# This function checks whether the inverse has already been calculated
# If it has already been calculated, then it will obtain the inverse from the cache
# If the inverse has not been calculated, then it will compute the inverse matrix and store i

cacheSolve <- function(x, ...) {
	sm <- x$getSolveMatrix()
	if(!is.null(sm)) {
		message("getting cached data")
		return(sm)
	}
	data <- x$get()
	sm <- solve(data, ...)
	x$setSolveMatrix(sm)
	sm
}

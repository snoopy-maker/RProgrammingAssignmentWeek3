# This function creates a special "matrix" object that can cache its inverse
# with following sub functions:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	#set the value for the matrix
	setMatrix <- function(y) {
		x <<- y
		m <<- NULL
	}

	#get the value of the matrix
	getMatrix <- function() x

	#set the value of the inverse
	setInverse <- function(inverse) m <<- inverse

	#get the value of the inverse
	getInverse <- function() m
	list(setMatrix = setMatrix, getMatrix = getMatrix,
		setInverse = setInverse,
		getInverse = getInverse)
}


# Calculates the inverse of the special "matrix" created with 
# the above function by first checking the inverse has already
# been calculated, then it retrieves inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse 
# of the data and sets the value of the inverse in the cache via 
# setInverse function.
cacheSolve <- function(x, ...) {
        
	m <- x$getInverse()
	#Check the inverse matrix is null
	if(!is.null(m)) {
      	message("getting cached data")
      	return(m)
    	}
    	data <- x$getMatrix()
    	m <- solve(data, ...)
    	x$setInverse(m)

	## Return a matrix that is the inverse of 'x'
    	return(m)
}

## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
# Get a special "matrix" object that can cache its inverse.
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


## Write a short comment describing this function

# Calculates the inverse of the special "matrix" created with 
# the above function.
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

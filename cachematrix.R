## Here we have to functions, which allow to save the time by the not
## computing the inverse of the matrix when it is not needed. When
## the inverse matrix is computed, it is saved in cache and one can
## get it with the help of "getInverse" function.

## This function creats a list of functions, which allow to
## set the value of a matrix, get the value of a matrix
## set the inverse of a matrix, get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL
	set <- function(y) {
		x <<- y
		inverseMatrix <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) {
		inverseMatrix <- inverse
	}
	getInverse <- function() inverseMatrix
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function allows to get the inverse of a matrix created
## with the above function. However, it first checks if the inverse
## matrix has already been computed. If so, it gets the inverse
## from the cache and does nothing more. Otherwise, it computes
## the inverse of a matrix with a help of "solve" function
## and sets the inverse matrix in cache with the help of
## setInverse function.

cacheSolve <- function(x, ...) {
	inverseMatrix <- x$getInverse()
	if(!is.null(inverseMatrix)) {
		message("getting cached data")
		return(inverseMatrix)
	}
	data <- x$get()
	inverseMatrix <- solve(data, ...)
	x$setInverse(inverseMatrix)
	inverseMatrix
}

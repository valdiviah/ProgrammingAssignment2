## Caching the inverse of a Matrix

## this function create a matrix object, and can calculate its inverse
## while also keeping track if it has been calculated so it can
## be later retrieved without having to calculate again

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x
	setInverse <- function(solve) i <<- solve
	getInverse <- function() i

	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## this function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i 
}

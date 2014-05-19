## These function cache the inverse of a matrix. It is assumed that the matrix is
## invertible. No checks are made to test the invertability of the matrix.

## A function to make a special kind of "invertible matrix" that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}	
	
	get <- function() x
	
	setinv <- function(inverse) inv <<- inverse
	
	getinv <- function() inv
	
	list(set = set, get = get,
		 setinv = setinv, getinv = getinv)
}


## A function to get the inverse of an invertible matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
	
	if(!is.null(inv)) {
		message("getting cached value")
		return(inv)
	}

	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

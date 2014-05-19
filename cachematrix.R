## These function cache the inverse of a matrix. It is assumed that the matrix is
## invertible. No checks are made to test the invertability of the matrix.

## A function to make a special kind of "invertible matrix" that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
	## a variable to hold the inverse of the matrix
        inv <- NULL
	
	## setters and getters
	## -- for the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}	
	
	get <- function() x
	
	## -- for the inverse of the matrix
	setinv <- function(inverse) inv <<- inverse
	
	getinv <- function() inv
	
	## list containing the possible functions on the special invertible matrix
	list(set = set, get = get,
		 setinv = setinv, getinv = getinv)
}


## A function to get the inverse of an invertible matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
	
	## check if the inverse if different from NULL
	if(!is.null(inv)) {
	## -- if yes, return the cached value of the inverse
		message("getting cached value")
		return(inv)
	}
	## -- if no: 
	## -- -- get the value of x
	data <- x$get()
	## -- -- compute the value of the inverse using the solve() function
	inv <- solve(data, ...)
	## -- -- cache the value of the inverse for x
	x$setinv(inv)
	## -- -- return the computed inverse
	inv
}

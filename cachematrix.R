##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function (y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}

##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. 

cachesolve <- function(x, ...) {
	m <- x$getmatrix()
## check if m has already been calculated, if true, return(m)
	if(!is.null(m)	{
		message("getting cached data")
		return(m)
	}
## if not calculated, solve the matrix and store the data.
	matrixdata <- x$get()
	m <- solve(matrixdata, ...)
	x$setmatrix(m)
	m
}

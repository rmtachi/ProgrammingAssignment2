## December 18, 2104
## R programming ass 2
## functions for creating and accessing a matrix and its inverse

## initializes a matrix and creates function handles to
## get/set matrix and inverse
## takes matrix, defaults to 1x1 NA matrix
makeCacheMatrix <- function(x = matrix()) {
	Inv <- NULL
	set <- function(y) {
		x <<- y
		Inv <<- NULL
	}
	get <- function() x
	setInv <- function(inv) Inv <<- inv
	getInv <- function() Inv
	list(set = set, get = get,
		 setInv = setInv,
		 getInv = getInv)

}


## takes list object created with makeCacheMatrix and retrieves inverse
## of matrix. Computes inverse if doesn't already exist
cacheSolve <- function(x, ...) {
    ret <- x$getInv()
	if(!is.null(ret)) {
		message("getting cached data")
		return(ret)
	}
	mat <- x$get()
## 'solve' computes x for Ax = b, set b = Identity to compute inverse
	ret <- solve(a=mat, b=diag(nrow(mat)), ...)
	x$setInv(ret)
	ret
}

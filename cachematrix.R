## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
	inv <- NULL
	set <- function(otherMtrx) {
		mtrx <<- otherMtrx
		inv <<- NULL
	}
	get <- function() mtrx
	setInv <- function(mInv) inv <<- mInv
	getInv <- function() inv
	list(set = set, get = get,
		 setInv = setInv,
		 getInv = getInv)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(cMtrx, ...) {
	inv <- cMtrx$getInv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mtrx <- cMtrx$get()
	inv <- solve(mtrx, ...)
	cMtrx$setInv(inv)
	inv
}

##	Programming assignment for Week 3, R Programming Class from Coursera, John Hopkins Univ.
##      Mar 4, 2016

##Student:  Steve Willis

##      Note:  Specification says to assume that a square, invertible matrix will 
##             be passed in as the matrix argument

makeCacheMatrix <- function(x = matrix()) {

##    Create Functions to save local copy of matrix inverse, and  
##    Use environment to store cached copy of matrix inverse

	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cacheSolve <-function(x,...) {

##    Check for previous calculation of inverse for this object
##    If object exists, return it, otherwise cacluate, store in 
##       env and return inverse to calling function


	inv = x$getInv()
	if (!is.null(inv)) {
	    message("getting cached Inv Matrix")
	    return(inv)
	}
        mat.data = x$get()

	inv <- solve(mat.data,...)
	x$setInv(inv)
	return(inv)
}


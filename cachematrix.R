## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## I figure that the makeCacheMatrix function will require a
## list of functions similar to the example: set, get, set inv,
## and get inv.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solvedSol) m <<- solvedSol
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
##
## This function checks to see if the inverse is already
## computed and either returns the previously compututed inv
## or computes the inv, saves the answer, then returns the inv


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

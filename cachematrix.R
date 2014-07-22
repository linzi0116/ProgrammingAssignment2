## This script contains two functions to calculate the inverse of an invertible matrix. 
## Calculate and cache the inverse for the first time, and retrieve from cache if it is already calculated.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) {message("set inverse"); m <<- inv}
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## It retrieve the inverse from cache if it has already been calculated and the matrix has not changed.

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

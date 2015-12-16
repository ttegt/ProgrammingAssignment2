## The following two functions create a way to avoid recalculating the inverse
## of a matrix once it has already been calculated. Code modeled on
## makevector/cachemean example.


## This function takes a matrix and creates a "special" matrix (really a list of functions) 
## that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will calculate the inverse of the special matrix
## created by makeCacheMatrix, or retrieve the cached inverse
## if it already has been calculated.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
       


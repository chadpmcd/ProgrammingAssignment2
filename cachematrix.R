## --------------
## The set of functions in this file allow one to get an inverse matrix using the
## solve function.  The inverse matrix is cached in the makeCacheMatrix environment
## and can be updated and pulled from the cacheSolve function environment.
## --------------

## --------------
## makeCacheMatrix
## This function creates sub functions for setting a matrix, getting a matrix,
## setting the inverse of a matrix, and getting the inverse of a matrix.
## The inverse matrix is stored in memory for later use.
## --------------

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## --------------
## cacheSolve
## Using the functions defined in makeCacheMatrix, checks to see if the inverse
## has been set/cached and returns it if it has or it will calculate the 
## inverse matrix and set it in cache.
## --------------


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


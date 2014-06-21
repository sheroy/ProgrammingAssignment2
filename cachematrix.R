## Set of functions to add the ability to cache the inverse of the Matrix and to 
## use the cached value while getting the inverse of a Matrix

## Adds the ability to cache the inverse of the matrix passed in as a paramter to this## function
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## Gets the inverse of the Matrix and caches the result if required.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (! is.null(i)) {
        message("Getting cached inverse")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}

## These functions make it possible to associate a matrix
## with its inverse, so that the inverse only needs to
## be computed once.

## Wraps a matrix object with helper functions for
## accessing a cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        ## Invalidate old cached value
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Wraps the solve() function, which computes the
## inverse of a matrix, to make use of a cached
## inverse value associated with the matrix if
## it exists

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Use cached value, if available
    inv <- x$getinverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## Author: Papp 8/21/2015
## Functions for caching an inverted matrix

## List of function definitions for caching a matrix

makeCacheMatrix <- function(x = matrix()) {
    ## start with empty matrix
    m <- NULL
    
    ## set initial matrix to y
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get initial matrix
    get <- function() x
    
    ## set inverted matrix
    setsolve <- function(solve) m <<- solve
    
    ## get inverted matrix
    getsolve <- function() m
    
    ## output functions for use
    list(
        set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}


## Invert matrix or return cached inverted matrix

cacheSolve <- function(x, ...) {
    ## attempt to get cached inverted matrix  
    m <- x$getsolve()
    
    ## if cached inverted matrix is not null, return cached inverted matrix
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if inverted matrix is null, get initial matrix
    data <- x$get()
    
    ## invert initial matrix
    m <- solve(data, ...)
    
    ## cache inverted matrix
    x$setsolve(m)
    
    ## return inverted matrix
    m
}

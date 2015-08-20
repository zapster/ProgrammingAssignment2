## R functions to create a matrix that caches its inverse matrix
## to avoid re-computation.

## Creates a 'CacheMatrix' object.
##
## @param x The 'raw' matrix.
##
## Methods:
##  $get()     Returns the underlaying matrix.
##  $set(x)    Sets the underlaying matrix.
##  $getInv()  Returns the inversed matrix.
##  $setInv(x) Sets the inverse matrix.
##
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() {
        m
    }
    setInv <- function(x) {
        inv <<- x
    }
    getInv <- function() {
        inv
    }
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Return a matrix that is the inverse of 'x'
##
## @param x A matrix that should be an instance created by `makeCacheMatrix`.
##
## Note: The inverse matrix will be cached in x (accessible via x$getInv).
##
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    message("calculating inverse matrix")
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}

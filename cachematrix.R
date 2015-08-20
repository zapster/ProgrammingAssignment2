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


## Alternative solution to the cache matrix problem:
##
## The problem with the two method solution is that objects
## created by makeCacheMatrix expose the $setInv method which
## allows manipulation of the inverted matrix. Everyone could
## use the method to set it to an arbitrary value with would
## make the object inconsistent (i.e. x$getInv() does not
## return the inverse matrix of x$get()).
##
## The solution proposed below moves the logic of cacheSolve
## into the $getInv member function. The $setInv member is
## no longer needed. The cached matrix is guaranteed to be
## consistent (if nobody messes around with the environment).
##
## In Object Oriented Programming this strategy might be
## called encapsulation and data protection.
makeCacheMatrix2 <- function(m = matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() {
        m
    }
    getInv <- function(...) {
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        message("calculating inverse matrix")
        inv <<- solve(m, ...)
        inv
    }
    list(set = set, get = get, getInv = getInv)
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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

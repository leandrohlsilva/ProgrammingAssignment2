## makeCacheMatrix creates a structure of a 'cacheable' matrix
## This is structure is a list containing 4 methods:
## get: retrieve the matrix
## set: set matrix data
## getinv: retrieve inverse of matrix
## setinv: set inverse of matrix

## makeCacheMatrix(x) - param x: matrix to be encapsulated
## makeCacheMatrix - return: 'cacheable' matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve(x, ...)
## Function expects a 'cacheable' matrix structure (x)
## and perform inverse calculations. If inverse is already
## calculated, return cached results.
## Any attempt to change structure's matrix data will automatically
## reset inverse cached data to NULL.

## cacheSolve - param x: 'cacheable' matrix
## cacheSolve - param ...: extra params to embbeded solve function
## cacheSolve - return: inverse of matrix x

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message('Getting cached data')
        return(inv)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}

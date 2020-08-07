################################################################
## A group of functions to wrap a matrix and cache the inverse.
################################################################

## Input matrix is wrapped in a list with named operations.
## The named operations include a get and set for the matrix
## as well as a get and set for the inverse.
##
## Input:
##   x: a matrix
##
## Output:
##   A wrapped matrix as a list named entries which are functions to interact
##   with it. The named functions are set, get, setinv, getinv.

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(xinv_) xinv <<- xinv_
    getinv <- function() xinv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv
    )
}

## With the wrapped matrix list, check to see if cache has the inverse and
## return it otherwise calculate it, store it, and return the value
##
## Input:
##   x: a matrix
##   ...: args that are passed to solve
##
## Output:
##   The inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        return(xinv)
    }
    mat <- x$get()
    matinv <- solve(mat, ...)
    x$setinv(matinv)
    return(matinv)
}

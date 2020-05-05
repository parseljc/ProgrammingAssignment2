## These functions are used to cache the inverse of a matrix. As long as the matrix does not change, the inverse can be 
## retrieved from the cache without recomputing it.

## This function takes a matrix as input.  It outputs a list object of four functions:
##   set - store (redefine) the matrix
##   get - get the matrix
##   setInv - store the inverse of the matrix
##   getInv - return the inverse of the matrix (or null if not yet calculated)

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) xInv <<- Inv
    getInv <- function() xInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function checks to see if the matrix inverse has been calculated, and returns it from the cache (with a message) if it has.
## If the inverse has not been calculated, it calculates it, caches it, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInv()
    if(!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInv(xInv)
    xInv
}

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly. This pair of functions, calculate the inverse of a matrix or
## retrieves it from cache if it has already been computed.

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse.
    
    matrixInverse <- NULL
    set <- function(y){
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(mInv) matrixInverse <<- mInv
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
    ##retrieve the inverse from the cache.
    
    ## Return a matrix that is the inverse of 'x'
    
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}

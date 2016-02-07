## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

## Creating a special matrix that has its inverse cached
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## Returning the cached inverse or calculating it and caching
cacheSolve <- function(x, ...) {
    i <- x$getInv()
    
    if(!is.null(i)) {
        message("Getting cached inverse")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}

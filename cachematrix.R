## Since matrix inversion can be computationally expensive/intensive,
## these two functions allow the user to cache a matrix inversion so
## it can be recalled instead of recalculated.

## The function makeCacheMatrix sets up a special "matrix" object that
## is capable of caching its inverse.  It does this by 1) setting the 
## values of the matrix, 2) getting the values of the matrix, 3) setting 
## the values of the inverse, 4) getting the values of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse){m <<- inverse}
    getInverse <- function()m
    list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## The function cacheSolve takes the special "matrix" that is outputted
## by makeCacheMatrix as its input.  If the inverse of the matrix has 
## not been calculated, this function calculates the inverse.  If the 
## matrix has not changed and the inverse has already been calculated
## and cached, this function retrieves and returns the cached inverse.


cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

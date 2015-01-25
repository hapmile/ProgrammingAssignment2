## These two functions help to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(newInverse) inverse <<- newInverse
        getInverse <- function() inverse
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. if the inverse has already been
## calculated(and the matrix has not changed), then the cacheSolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                  message("getting cached data")
                  return(inverse)
        }
        newInverse <- solve(x$get())
        x$setInverse(newInverse)
        newInverse
}

## A pair of functions that allow caching of matrix
## inversions

## Creates an object which holds a matrix and 
## it's inverse, with function for getting and setting them.
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    
    set <- function(matrix) {
        x <<- matrix
        matrixInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrixInverse <<- inverse
    getinverse <- function() matrixInverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of the CacheMatrix passed to the
## function, cahing the result.
## Assumes the matrix is square.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    
    inverse
}

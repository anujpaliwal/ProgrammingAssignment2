## Caching the inverse of Matrix:
##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cache data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}
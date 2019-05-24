## Caching the Inverse of a Matrix

## This function accepts a matrix as an argument and creates a matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function accepts the matrix returned by makeCacheMatrix function as its argument and calculates its inverse. 
## Next time when the cachesolve function is called for the same matrix, the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## The following  functions caches the inverse of a matrix and returns its inverse

## makeCacheMatrix creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix 'm' that is the inverse of 'x' if it is cached
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached Inverse")
                return(m)
        }
        ## Calculate, set cache and return a matrix 'm' that is the inverse of 'x' 
        ## if it is not cached
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

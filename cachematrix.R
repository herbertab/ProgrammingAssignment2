## The functions below allow you to cache and retrieve the inverse of a
## matrix instead of calculating it every time it is requested.

## This function creates a special "matrix" with functions to set / get the
## matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
			 
}


## This function retrieves the inverse of a special matrix, if available, 
## otherwise calculates and stores it.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("gettingcached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}

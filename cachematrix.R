## These functions create and work on a special type of matrix. In order to 
## save computation time, it stores the inverse of the matrix once it is 
## solved initially. The inverse can be recalled without being recalculated.


## This function creates a special matrix object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function retrieves the matrix inverse. If the inverse has not yet been
## calculated, it calculates the inverse.
cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    
    m
    
}

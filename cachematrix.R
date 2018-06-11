## Creates an object that contains the inverse of a matrix
## so that this cached inverse can be used instead of
## recalculating the inverse each time it is needed

## Creates an object to contain matrix, inverse, and functions
## to set and retrieve inverse

makeCacheMatrix<- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If inverse has been solved and cached, return cached inverse
## If inverse has not been cached, calculate inverserm

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

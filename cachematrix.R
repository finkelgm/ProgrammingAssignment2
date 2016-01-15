## Here are the pair of functions that cache the inverse of a matrix.

## This function is just like the function from example with two differenses:
#  1. It compares new and old matrix? and if these matrixes are identical, do nothinig
#  2. Instead of "mean" it uses "inverse"

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        if (!identical(x,y)) {
            x <<- y
            m <<- NULL
        }
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is just like the function from example, but instead of "mean" it solves the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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



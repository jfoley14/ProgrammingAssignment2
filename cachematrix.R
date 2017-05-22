## These functions provide a more efficient way of calculating the inverse of
## a matrix by storing the matrix and its inverse in a common object once the
## inverse of the matrix has been calculated

## makeCacheMatrix uses setter and getter functions to set and get a
## CacheMatrix's matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSovle takes a CacheMatrix and returns the inverse of the matrix in
## the given CacheMatrix, as well as sets the inverse of this matrix in the
## CacheMatrix if it hasn't already been set

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

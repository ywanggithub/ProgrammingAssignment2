## makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.

## Usage: a<-makeCacheMatrix(x). output a is a list of four functions 
## getm, setm, getinverse, setinverse, input x is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    setm <- function(y) {
        x <<- y
        minv <<- NULL
    }
    getm <- function() x
    setinverse <- function(solve) minv <<- solve
    getinverse <- function() minv
    list(setm = setm, getm = getm,
         setinverse = setinverse,
         getinverse = getinverse)  
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$getm()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}

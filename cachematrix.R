# Coursera RPROG 031 - Programming Assignment 2 Submission

## As described in the introduction to "Programming Assignment 2", the
## two functions below "cache" (or save) the value of potentially complex,
## and therefore time-intensive, calculations for later recall and use
## by taking advantage of R's lexical scoping rules.

### The function "makeCacheMatrix" stores a given square (invertible
### or non-singular) matrix that can also cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
        
        
### The function "cacheSolve" checks to see if the inverse of the matrix 
### given by "makeCacheMatrix" (above) has already been calculated and stored,
### and then returns this inverse matrix. If it has already been calculated,
### the inverse matrix is returned from the cache, but if it has not been calculated,
### "cacheSolve" calculates the inverse of the given matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

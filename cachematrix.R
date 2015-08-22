# Coursera RPROG 031 - Programming Assignment 2 Submission

## As described in the introduction to "Programming Assignment 2", the
## two functions below "cache" (or save) the value of potentially complex,
## and therefore time-intensive, calculations for later recall and use
## by taking advantage of R's lexical scoping rules.

### The function "makeCacheMatrix" stores a given square (invertible
### or non-singular) matrix that can also cache its inverse.

## Please Note: This code is modified from the sample code for "Caching the Mean of 
## a Vector", provided by Course Instructor R. Peng in the file forked from: 
## https://github.com/rdpeng/ProgrammingAssignment2 on 21 August 2015.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(inverse) i <<- inverse
        geti <- function() i
        list(set = set, get = get, seti = seti, geti = geti)
}
        
        
### The function "cacheSolve" checks to see if the inverse of the matrix 
### given by "makeCacheMatrix" (above) has already been calculated and stored,
### and then returns this inverse matrix. If it has already been calculated,
### the inverse matrix is returned from the cache, but if it has not been calculated,
### "cacheSolve" calculates the inverse of the given matrix.

cacheSolve <- function(x, ...) {
        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$seti(i)
        i
}

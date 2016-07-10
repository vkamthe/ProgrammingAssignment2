## Programming Assignment 2 - 10-Jul-2016

## Description makeCacheMatrix- create an object of type makeCacheMatrix having 4 functiosn - 
## get,set, setSolve, getSolve - for a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setSolve <- function(solve) {
                m <<- solve
        }
        getSolve <- function() {
                m
        }
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## Description cacheSolve - compute inverse of a function using solve function
## and storing it in cache for faster computation next time 
cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}

# Programming Assignment 2

## The assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse. It is really a list containing a function to
   ### set the value of the matrix
   ### get the value of the matrix
   ### the value of the inverse
   ### the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y       ## "<<-" allows modifying variables in the main levels.
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function  calculates the inverse of the special matrix previously created. 
## However, if the inverse has already been calculated it gets the inverse from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)   ## solve(X) returns the inverse of a square matrix.
        x$setinverse(m)
        m
}

## Testing the functions

Original <- matrix(1:7, nrow = 3, ncol = 3) ## Creating a matrix
Special <- makeCacheMatrix(Original)
cacheSolve(Special)


## Code to cache inverse matrices in R

## This function creates a list with 4 elements:
## 1 -  A function to set the value of the matrix (x).
##      This matrix can also be set when calling makeCacheMatrix,
##      e.g. makeCacheMatrix(a) instead of makeCacheMatrix()[[1]](a).
## 2 -  A function to return the stored matrix (x).
## 3 -  A function to store a second matrix (m).
## 4 -  A function to return the second matrix.
##
## The second matrix is in principle arbitrary, but cacheSolve()
## will use it to store the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { # First function
        x <<- y
        m <<- NULL # Resets the inverse since the matrix has changed
    }
    get <- function() x # Returns the matrix
    setinv <- function(inv) m <<- inv # Sets the inverse
    getinv <- function() m # Returns the inverse
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## The following function checks a list ("matrix") created
## by makeCacheMatrix(). If there is an inverse matrix stored, it
## will return the inverse matrix. Otherwise, it will calculate the
## inverse matrix, store it, and return it.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    # Check for cache and return if true
    if(!is.null(m)){
        message("Using cached data...")
        return(m)
    }
    # Calculate inverse
    data <- x$get()
    m <- solve(data, ...)
    # Store it for next time
    x$setinv(m)
    m
}

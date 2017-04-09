# cachematrix.R
#
# Functions to invert a square matrix and cache the result
# Once the matrix is inverted once using cacheSolve(), future
# calls will simply return the previously computed matrix inversion


# Creates a matrix object with subfunctions to 
# invert the matrix and cache the result
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set.inverse <- function(inv) inverse <<- inv
    get.inverse <- function() inverse
    list (set = set, get = get,
          set.inverse = set.inverse,
          get.inverse = get.inverse)
}


# When called with a specially created 'cached matrix'
# object (from above), will first check cache to see
# if result has already been computed. Only inverts
# matrix if the cache is NULL
cacheSolve <- function(x, ...) {
    inverse <- x$get.inverse()
    if(!is.null(inverse)) {
        message("Getting cached inverted matrix...")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set.inverse(inverse)
    inverse
}

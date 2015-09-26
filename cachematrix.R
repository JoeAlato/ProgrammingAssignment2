## Functions create a special object that stores a matrix and caches its inverse. 

## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL ## set default value for inverse matrix
    set <- function(y) {
        x <<- y ## use input to substitute source matrix
        sol <<- NULL ## the old inverse matrix is not useful anymore, so delete it
    }
    get <- function() x ## returns the source matrix
    setsolve <- function(solve) sol <<- solve ## sets the inverse matrix (does not calculate it!)
    getsolve <- function() sol ## returns the inverse matrix
    ## return list of all 4 functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function calculates the inverse of the "matrix" created by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
    sol <- x$getsolve()
    if(!is.null(sol)) { ## if the inverse has already been calculated
        message("getting cached data")
        return(sol) # then the inverse is retrieved from the cache
    }
    ## if the inverse has not been calculated yet
    data <- x$get() # then the source matrix is obtained
    sol <- solve(data, ...) # and the inverse matrix is calculated
    x$setsolve(sol)  # and then it is stored in cache
    sol ## Return a matrix that is the inverse of 'x'
}
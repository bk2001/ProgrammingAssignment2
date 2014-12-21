## These two functions (makeCacheMatrix and cacheSolve) work together
## to make a cached inverted matrix so the operation of inverting the
## matrix is only performed once but the result can be called many
## times.


## This function returns the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) m <<- solve
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)

}


## This function returns the inverted matrix by calling the function
## created with makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}

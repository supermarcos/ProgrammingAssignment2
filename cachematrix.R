# makeCacheMatrix is a "function" that declares a bunch of properties for an "object", so to speak...
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # set is the setter, the function that assigns y to x and clears inv on the upper level context
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get is the function that returns x, the matrix
    get <- function() x
    
    # this setter assigns inverse to inv on the upper level context
    setInverse <- function(inverse) inv <<- inverse
    
    # this getter just returns the variable inv
    getInverse <- function() inv
    
    # exposed functions of x (set, get, setInverse and getInverse)
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve is the function that, given a matrix as an argument will return the inverse (wheter it is cached or not)

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    # if there is something already in inv, then return that,
    # if nothing in inv, we have to calculate the inverse of that matrix
    if (is.null(inv)) {
        # inverse the matrix calling the function setinverse of x
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
    } else {
        message("the data is cached")
    }
    # this will return the cached data when is.null(inv) is not TRUE
    return(inv)
}

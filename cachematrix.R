## makeCacheMatrix() & cacheSolve() #######################

# makeCacheMatrix(): creates a special "matrix" object that
# cache its inverse. It returns a list containing the 
# following functions:
        # set the matrix
        # get the matrix
        # set the inverse 
        # get the inverse

makeCacheMatrix <- function(x = matrix()) {
    # matrix() is invertible and squared 
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        # assigns a value to an object in an environment
        # that is different to the current one. 
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# cacheSolve(): computes the inverse of the special "matrix"
# returned by makeCacheMatrix(). If it has already been 
# calculated the function returns the inverse stored in the 
# cache

cacheSolve <- function(x, ...) {
    # Input x is the output of makeCacheMatrix()

    inv <- x$getInverse()
    # in is defined to be the inverse stored in cache 
    # (if exists)
    if (!is.null(inv)) {
        # checks whether inverse already exists
        message("getting cached data")
        return(inv)
    }
    # otherwise calculates the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    # sets the value of the inverse in the cache via 
    # setInverse function
    x$setInverse(inv)
    inv
}

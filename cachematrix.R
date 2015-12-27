# filename: cachematrix.R
# This script uses lexical scoping.
# It creates a special "matrix", which is a list containing a function to
#   - set the value of the matrix
#   - get the value of the matrix
#   - set the value of the inverse matrix
#   - get the value of the inverse matrix
# Then it solves for the matrix by looking at the cache data first.

# Usage example:
# > source('cachematrix.R')
# > m <- makeCacheMatrix(matrix(c(2, 0, 0, 4), c(2, 2)))
# > cacheSolve(m)
# [,1] [,2]
# [1,]  0.5 0.00
# [2,]  0.0 0.25

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}

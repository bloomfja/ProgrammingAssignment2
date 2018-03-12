#creates special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#calculates the inverse of the special "matrix". It checks to see if the iverse has already been calculated, if so it gets the inverse from the cache and sets the obtained value instead of restarting the calculation.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        Matrix <- x$get()
        i <- solve(Matrix, ...)
        x$setinverse(i)
        i
}

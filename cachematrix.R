## The following functions cache the inverse of a matrix rather than compute it repeatedly.
## The following two functions are used to cache the inverse of the matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function returns the inverse of the matrix, but first checks
## if the inverse has already been computed.  If already computed it gets
## the result and skips the computation.  If not, it computes the inverse, sets
## the value in the cache via setinverse function.

## This function assumes the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

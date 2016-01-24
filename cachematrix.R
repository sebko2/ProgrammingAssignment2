## Below are a pair of functions which creates special matrix object which cache matrix inverse.
## This may save a lot of time when user computes inverse many times and matrix didn't change.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()
                x
        setinverse <- function(solve)
                m <<- solve
        getinverse <- function()
                m
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Function computes inverse matrix or return it from cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m ## Return the inverse of 'x'
}

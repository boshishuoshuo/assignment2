## If the inverse is calculated, it is stored. It won't be calculated
## next time, and it can be found from the storage.


## makeCacheMatrix generates a list to store calculated matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        }
}

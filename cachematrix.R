## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## init for the variable
        m <- NULL
        ## uesd to set the matrix and set the m as NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## used to return the matrix
        get <- function() x
        ## used to set and return the inverse value of the matrix
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        ## list the functions 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## If there is a inverse value in the cache, return it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If not, calculate it again.
        data <- x$get()
        m <- solve(data, ...)
        ## Set the inverse value to the cache.
        x$setinverse(m)
        ## Return the result.
        m
}

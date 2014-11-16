## this set of functions enables the caching of the inverse of a matrix

## in this function we build a set of function that enables storing the inverse value and retrieving it when needed. if the matrix changes,
# the inverse is set to null

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
	get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## in this function we first try getting the cached inverse of the matrix, if it is not calculated, we call the solve function and then store the result
# in the cache matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

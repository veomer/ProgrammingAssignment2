## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## These pair of functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "vector", which is really a list containing functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
		    ## the <<- operator is used to assign a value to an object in an environment that is different from the current environment.
		    set <- function(y) {
		      x <<- y
		      i <<- NULL
		    }
		    get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		    i <- x$getinverse()
        if(!is.null(i)) {
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

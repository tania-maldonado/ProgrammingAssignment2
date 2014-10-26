## This is the second  Programming Assignment. It's to write a pair of functions
## that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse:
##       1.set the value of the matrix
##       2.get the value of the matrix
##       3.set the value of the inverse
##       4.get the value of the inverse

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



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
## The  function solve(X)  is used to calculate the inverse matrix and it is 
## assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        message("calculating the inverse")
        x$setinverse(m)
        m       
        
}


## This function creates a matrix,which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
                
        get <- function()x
        setinverse <- function(solve) m<<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## It calculates the inverse of the special matrix ,it first check whether 
## inverse has already been calculated. If so it gets the inverse from the
## cache and skips the computaion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}

## Assignment: Caching the Inverse of a Matrix
## Sean Bearden, 2016-07-26
## Creating two funtions that allow for a matrix inverse to be retrieved without
## computing it again if it has been already computed.

## Given an invertible matrix, makeCacheMatrix will cache its computed inverse.
## A list of 4 functions is return. 

makeCacheMatrix <- function(x = matrix()) {
        # assume matrix is always invertible.
        # solve(x) returns inverse of matrix x.
        
        m<-NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        } 
        
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse, 
             getInverse = getInverse)
        
}


## The function cacheSolve will determine if matrix x has a computed inverse. 
## If true, then it returns that Inverse. If false, then that inverse will be 
## computed and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

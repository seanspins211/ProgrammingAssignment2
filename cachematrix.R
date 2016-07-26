## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #assume matrix is always invertible.
        #m <-solve(x)
        
        m<-NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        } 
        
        get <- function() x
        setInverse <- function(solve) m<<-solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse, 
             getInverse = getInverse)
        
}


## cacheSolve will determine if matrix x has a computed inverse. If true, then it returns
## that Inverse. If false, then that inverse will be computed and returned.

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

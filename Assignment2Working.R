
## These functions create a special matrix function that can cash its own inverse and 
## a function to compute the inverse of a matrix
##
## Matrix inverses are often costly to compute and these functions allow the inverse to be 
## 'cached' for later use



## This function creates a special 'matrix' function that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}





## This function computes the inverse of a matrix
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}        


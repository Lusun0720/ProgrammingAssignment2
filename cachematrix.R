## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL						                   
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x					              
        setinv<-function(solve) inv <<- solve		       
        getinv <- function() inv				         
        list(set = set, get = get,
             setinv = setinv,	
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        m <- x$getInversematrix()
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data) %*% data
        x$setInversematrix(m)
        m
}

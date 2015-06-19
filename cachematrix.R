## A pair of functions to cache the inverse of a matrix
## HOW TO RUN
## x <- matrix(c(4,3,3,2),2,2)
## a <- makeCacheMatrix(x)
## cacheSolve(a)

## Creates an inverse cache matrix
## Returns a list of fuctions that can set the values of matrix and its inverse
## The matrix 'inv' is the cache inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse matrix
        inv <- NULL
        
        ## Set matrix
        set <- function(y = matrix()){
                x <<- y
                inv <<- NULL  
        }
        
        ## Returns matrix
        get <- function() x
        
        ## Sets inverse of matrix x in matrix inv
        setInverse <- function(inverse) inv <<- inverse
        
        ## Returns inverse of the matrix
        getInverse <- function() inv
        
        ## Return a list of methods 
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes inverse of matrix passed to MakeCacheMatrix
## Returns inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## return inverse if already set
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## Get matrix from list object function
        data <- x$get()
        
        ## Calculate inverse
        inv <- solve(data,...)
        
        ## Set inverse using list object function
        x$setInverse(inv)
        
        ## Return inverse
        inv
}

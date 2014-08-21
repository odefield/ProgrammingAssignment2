## A pair of functions that cache the inverse of a matrix.
## The functions assume that the matrix supplied is invertible
## an invertible matrix A has a determinant |A| != 0, always square matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {                ## sets the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                 ## retrieves the value of the matrix
    setinv <- function(inv) m <<- inv   ## writes the inverse to the cache
    getinv <- function() m              ## retrieves the cached inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and
##  the matrix has not changed), then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', solve(A) for square matrices

    m <- x$getinv()
    if(!is.null(m)) {                  ## checks if the inverse is cached
        message("getting cached data") 
        return(m)                      ## returns the cached value and exits the function
    }
    data <- x$get()                    ## if inverse is not cached, this part will be executed
    m <- solve(data, ...)
    x$setinv(m)
    m
}

## This function creates a special "matrix" object that can 
## cache its inverse

## Input of matrix needed

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(x) {
                m <<- x
                inv <<- NULL
        }
        get <- function() m
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

	
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'mat'
        invI <- m$getinv()
        if (!is.null(invI)) {
                message("getting cached data")
                return(inv)
        }
        mI <-  m$get()
        inv <- solve(mI, ...)
        m$setinv(inv)
        inv
}

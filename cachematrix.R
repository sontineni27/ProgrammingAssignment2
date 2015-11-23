## Caching the Inverse of a Matrix


makeCacheMatrix<- function(x = matrix()) { 
        m <- NULL  ## assigning NULL to M
        set <- function(y) {  ## assigning function(y) to set
                x <<- y 
                m <<- NULL
        }
        get <- function() x     ## get the matrix
        setInverse <- function(inverse) m <<- inverse ## invert Matrix
        getInverse <- function() m    ## get inverse Matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)  ## return vales to the function
}


cacheSolve <- function(x, ...) {
        m <- x$getInverse()  ## intialize m to Matrix from cache
        if(!is.null(m)) {   ## checking if cache has matrix or not
                message("getting cached data")
                return(m)   ## display inverse matrix in cache
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
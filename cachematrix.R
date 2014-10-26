## Finds the inverse for a matrix; if the matrix inverse has been previously
## calculated, it will pull the inverse matrix from cache.  Otherwise, it will
## calculate the inverse matrix, then store it to cache.

## This function extends the matrix object to provide getter and setter functions
## to allow the matrix to be cached.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrx) m <<- matrx
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function provides the inverse matrix for a given matrix.  It first tries
## to find the matrix in cache; if it finds it, it gets it from cache and returns
## it.  Otherwise, it calculates the inverse matrix, and stores it to cache, then 
## returns it.

cacheSolve <- function(x, ...) {
        
        ## Try to find the matrix in cache
        m <- x$getmatrix()
        
        ## if the matrix is found (null is NOT returned), the message that you 
        ## are getting the cached version, then return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix and assign it to 'data'
        data <- x$get()
        
        ## Solve for the matrix 
        m <- solve(data, ...)
        
        ## Write the inverse matrix to cache
        x$setmatrix(m)
        
        ## Return the inverse matrix
        m
}

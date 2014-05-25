## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## set 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get 
        get <- function() x
        ## set the value of solve
        setmatrix <- function(solve) m <<- solve
        ## get the value of solve
        getmatrix <- function() m
        list (set = set, get = get, 
              setmatrix = setmatrix, 
              getmatrix = getmatrix)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix,...)
        x$setmatrix(m)
        m
}

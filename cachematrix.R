# This assignment includes the following functions:
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix" object that can cache its inverse.
    # Input "x" is a square matrix (assumed to be invertible).
    
    invm <- NULL
    
    #Sets the value of the vector
    set = function(y) {
        x <<- y
        invm <<- NULL
    }
    
    #Gets the value of the vector
    get = function() x
    
    #Sets the value of the inverse
    setinvm = function(inverse) invm <<- inverse
    
    #Gets the value of the inverse
    getinvm = function() invm
    list(set = set,
    get = get,
    setinvm = setinvm,
    getinvm = getinvm)
}

cacheSolve <- function(x, ...) {
    # Creates inverse of matrix evaluated by makeCacheMatrix().
    # Input "x" is output of makeCacheMatrix()
    
    invm = x$getinvm()
    
    #Evaluates if mean has already been calculated
    if (!is.null(invm)){
        
        #If yes, retrieve inverse from cache
        message("getting cached data")
        return(invm)
    }
    
    #If no, calculate the inverse
    invm.data = x$get()
    invm = solve(invm.data, ...)
    
    #Sets the value of the inverse in the cache via "setinvm" function
    x$setinvm(invm)
    
    return(invm)
}

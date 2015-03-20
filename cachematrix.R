## Programming assignment 2: Caching the Inverse of a Matrix
## 


## Creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL              # Var to store inverse
    set <- function(y) {   # Initialize the special "matrix" and  special "var" to store the matrix and it's inverse
        x <<- y
        i <<- NULL
    }
    
    # Function to get the matrix
    get <- function() x    
    
    # Function to store the inverse
    setinv <- function(solve) i <<- solve
    
    # Function to return the stored inverse
    getinv <- function() i
    
    # Return value of call to makeCacheMatrix - list internal functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}  



## Returns the inverse of the special "matrix" from cache if it exists or calculates

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()     # Get cached value
    if(!is.null(i)) {   # If cache value is not NULL return it
        message("returning inverse from cache")
        return(i)
    }

                           # otherise
    data <- x$get()        # Get the matrix to compute the inverse on
    i <- solve(data, ...)  # Compute the inverse
    x$setinv(i)            # Store result in cache
    i                      # return the result
} 
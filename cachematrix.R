# R Programming - Assignment 2
# Last updated: 07/26/14
# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# The cacheSolve function computes the inverse of the special "matrix" returned by the makeCacheMatrix function.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
# The cacheSolve function uses R's internal solve() function to calculate the inverse of a function.
# The following resources were helpful and used in developing this solution:
#  1. Richard Ambler's post on the R Programming forum titled: "Assignment 2: What the heck?!". Forum URL: https://class.coursera.org/rprog-005/forum/thread?thread_id=139.
#  2. Gregory D. Home's post on the R Programming forum in response to the "Explanation of makeVector and cachemean" post. Forum URL: https://class.coursera.org/rprog-005/forum/thread?thread_id=482.
#  3. Sky King's post on the R Programming forum, where he provided comments on the functionality of the makeVector and cachemean functions. Forum URL: https://class.coursera.org/rprog-005/forum/thread?thread_id=1121.
# Examples for running from command line:
# 1. a <- makeCacheMatrix()
# 2. a$set(matrix(1:4, 2, 2))
# 3. a$get()
# 4. a$getinverse()
# 5. cacheSolve(a)
# 6. cacheSolve(a)

# makeCacheMatrix takes x, a matrix as input.
# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {    
    # Initialize mat (variable for matrix) to NULL during the first call to makeCacheMatrix.
    # Required because if getinverse() is called immediately after the makeCacheMatrix function is constructed, without a call to setinverse(),
    # we need to first calculate the inverse in cacheSolve().
    mat <- NULL
    
    # Function to set a new value for the underlying matrix.
    # This invalidates the cached inverse, mat
    set <- function(y) {
        # The <<- operator is used to set the value of x and mat because we want to modify x and mat defined in the parent environment (created 
        # when makeCacheMatrix() was first called), not in the environment local to set(), in which x and mat are undefined.
        x <<- y
        
        # Reset mat to NULL since we are modifying the underlying matrix and the cached value is no longer valid.
        mat <<- NULL
    }
    
    # Get function for underlying matrix.
    get <- function() {
        return(x)
    }
    
    # Set the inverse of the matrix x.
    # Called by cacheSolve.
    setinverse <- function(matinverse) {
        # The <<- operator is used again because we want to modify the mat defined in the enclosing function makeCacheMatrix(), not the mat local to setinverse(),
        # which would be undefined (or would create a new variable m local to the scope of setinverse()).
        mat <<- matinverse
    }
    
    # Returns the matrix inverse.  Will be null if setinverse has not been called or if set is called after the last call to setinverse.
    getinverse <- function() {
        return(mat)
    }
    
    # Function pointers.
    # Return value of the makeCacheMatrix function is a list of functions that we want to expose as public.
    # These are accessed with the $ operator.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



# The cacheSolve function computes the inverse of the special "matrix" returned by the makeCacheMatrix function.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
# The cacheSolve function uses R's internal solve() function to calculate the inverse of a function.
# cacheSolve returns a matrix that is the inverse of x, matrix.
cacheSolve <- function(x, ...) {
    # Get the inverse of the matrix defined inside x.
    # We can use the $ operator to access the function since it was defined in the list of function pointers returned by the call to makeCacheMatrix.
    mat <- x$getinverse()
    
    # If we've already computed the inverse and stored it via setinverse(), and have not invalidated the cache by calling set(), return the cached version of the matrix.
    if(!is.null(mat)) {
        message("getting cached data...")
        
        # Since the cached version is good, just return it.
        return(mat)
    }
    
    
    # Either we haven't computed the cached version yet, or we've called set() previously and invalidated the cache.
    
    # Call get() to get the underlying matrix.
    data <- x$get()
    
    # Calculate the inverse of the underlying matrix, passing with it any varargs passed to cacheSolve
    mat <- solve(data, ...)
    
    # Now set the inverse of x so we can cache it and avoid having to needlessly recompute it.
    x$setinverse(mat)
    
    # Return the cached matrix inverse
    mat
}
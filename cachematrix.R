

## makeCacheMatrix:
## To facilitate this caching, you first create a special
## matrix that will help us with this by using the
## makeCacheMatrix function.  The input into this function
## is simply a variable of type matrix.
##
## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
# Following the same format as the assignment example
	# Creating a makeCacheMatrix object will consist of
	# four functions encapsulated in a list
	# 1. set the matrix
	# 2. get the matrix
	# 3. set the inverse of the matrix
	# 4. get the inverse of the matrix

	# Initially set to NULL
	# Changes when the user sets the value
m <- NULL
    # set function
    # Sets the matrix itself but not the inverse
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    # get function
    # Gets the matrix itself but not the inverse
    get <- function() x
    
    # Manually set the inverse
    setinverse <- function(solve) m <<- solve
    
    # Get the inverse
    getinverse <- function() m
    
    # Encapsulate into a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve:
## Once you create this matrix, you use the cacheSolve
## function to compute the inverse and cache the result
##
## If you try using cacheSolve again on the same special
## matrix, then the pre-computed result is obtained, thus
## avoiding any recomputation.  An informative message
## will be shown in the command prompt when the pre-computed
## result is returned instead.
##
## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)
## s <- cacheSolve(m)
## print(s)
## s should return:
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## s2 <- cacheSolve(m)
## This should display a "Getting cached data for inverse Matrix" message
## print(s2)
## s2 should return
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Get the current state of the inverse and see if it
        ## has been computed yet
        m <- x$getinverse()
        
        ## Check if the inverse of the Matrix already exist in cache
    if(!is.null(m)) {
        ## Simply return the computed inverse
      message("Getting cached data for inverse Matrix")
      return(m)
    }
    
       ## If it hasn't...
       ## Get the matrix itself
    data <- x$get()
    
       ## Find the inverse
    m <- solve(data, ...)
    x$setinverse(m)
    
       ## Return this new result
    m
}

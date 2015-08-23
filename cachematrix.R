## The following pair of functions cache the inverse of a matrix.
## This allows the inverse of a matrix to be looked up in cache rather than recomputing every time.

## The following function makecachematrix creates a list containing a 
## function to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
              I <- data.frame()
            set <- function(y) {
              x <<- y
              I <<- data.frame()
           }
           get <- function() x
           setinverse <- function(solve) I <<- solve
           getinverse <- function() I
           list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## Following function calculates the inverse of the matrix created with 
## above function. It first checks if inverse has already been calculated.
## If yes, it gets the inverse from cache and skips computation. Else, it
## calculates the inverse and stores in the cache via setinverse function.

cacheSolve <- function(x, ...) {
          I <- x$getinverse()  ## Return a matrix that is the inverse of 'x'
          if(!is.null(I)) { 
                        message("getting cached data")
                        return(I)
          }
          data <- x$get()
          I <- solve(data, ...)
          x$setinverse(I)
          I
}

## Since matrix inversion is a costly computation, these functions allow you to cache the inverse of a 
## matrix rather than compute it repeatedly.

## This function creates a special matrix which is really a list containing a function to set the value
## of the matrix, get the value of the matrix, set the value of it's inverse and get the value of it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates inverse of a special matrix created by the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse
##from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and 
##sets the value of the inverse in the cache via the setinverse function. It is assumed that the
##supplied matrix is always invertable.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}

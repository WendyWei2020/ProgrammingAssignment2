## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function makeCacheMatrix is to cache a matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function() inv <<- solve(x) 
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  



## Write a short comment describing this function
##The function is to calculate the inverse of the matrix cache by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- inv(data, ...)
    x$setInverse(inv)
    inv
  
}

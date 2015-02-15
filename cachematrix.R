## implement a function special matrix that caches
## its inverse and and a function that actually calculates
## the inverse of a matrix based on the cached value

## function that creates a special matrix that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## function that computes inverse

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(is.matrix(inverse)){
          message("getting cached data")
          return(inverse)
        }
        input <- x$get()
        inverse <- solve(input)
        x$setInverse(inverse)
        inverse    
}

## The purpose of these functions is to compute the inverse of a matrix and cache the result for potential further use, avoiding repeated computation eating up processing space.  

## The makeCacheMatrix function generates a list of functions to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function takes the role of actually computing the inverse of a matrix should the inverse not be available already in the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  return(inv)
  
        ## Returns a matrix that is the inverse of 'x'
}

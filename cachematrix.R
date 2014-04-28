makeCacheMatrix <- function(x = matrix()) {
## set the value of the matrix X
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  ## get the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix X produced by the makeCacheMatrix function.
## If the cached version is available, cacheSolve retrieves it.
## If not, cacheSolve computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix        
  i <- x$getinverse()
  
  ## check if there is the matrix   
  if(!is.null(i)) {
    message("Cached data...")
    return(i)
  }
  ## if not: get the inverse of the matrix   
  data <- x$get()
  i <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(i)
  i
}

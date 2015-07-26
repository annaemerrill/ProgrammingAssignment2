## These functions cache the inverse of a matrix to avoid repetitive computation

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function (y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) s <<- solve
  getMatrix <- function() s
  list (set = set, get = get, 
        setMatrix = setMatrix,
        getMatrix = getMatrix)
}


## Computes inverse of special matrix
## If inverse has already been computed, retries cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getMatrix()
  if (!is.null(s) && x == s){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(x)
  x$setMatrix(s)
  s        
}

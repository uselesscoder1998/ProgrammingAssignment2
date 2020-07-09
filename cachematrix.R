## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Set the Matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Get the Matrix
  get <- function() x
  ##Set the inverse
  setInverse <- function(inverse) i <<- inverse
  ##Get the inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Solve the inverse
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}

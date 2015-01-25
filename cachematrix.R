## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Makes a 'matrix' that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) m <<- inv
  
  getInverse <- function() m
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Returns the inverse of the 'matrix'
## If the inverse has not been calculated yet, it will find and save it.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  ## if the inverse is already known return it
  if(!is.null(m)) {
    message("getting matrix inverse")
    return(m)
  }
  ## find the inverse and return it.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
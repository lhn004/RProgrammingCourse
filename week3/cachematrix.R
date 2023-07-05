## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  ## Creates a special “matrix” object that can cache its inverse
  ## x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  
  inv <<- NULL
  set <- function(y) {
    # assign a value to an object in an environment 
    # different from the current environment.
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


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) { # if the inverse has already been calculated
    message("getting cached data")
    return(inv)
  }
  
  # if not, calculate the inverse 
  data <- solve(x$get())
  inv <- solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function
  x$setinverse(inv)
  
  inv
}
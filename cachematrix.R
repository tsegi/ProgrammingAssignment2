## makeCacheMatrix creates a special "matrix" with list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # variable that caches the inverse
  inv <- NULL
  
  # function that sets the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # function that gets the value of the matrix
  get <- function() x
  
  # function that sets the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # function that gets the value of the inverse of the matrix
  getinverse <- function() inv
  
  # put the functions in a list and return
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}





## cacheSolve first checks whether a cached inverse for the 
## matrix "x" is available, if so return that. Otherwise it 
## calls solve() to compute the inverse, cache the computed 
## value and return.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  message("computing inverse")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

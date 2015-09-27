## Programming Assignment 2 [R programming] --Cashing  Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}

# Calculate the Inverse of "makeCasheMatrix". If the inverse has allready been calculated  
# it will retrieve that one.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)                     ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}
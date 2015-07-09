## The 2 functions below work together to compute the inverse of a matrix and cache 
## the result to avoid repeated resources-intense computation.

## makeCacheMatrix is a list of 4 functions, which allowed a matrix to be stored and 
## its inverse cached, and for cacheSolve to retrieve the value stored.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve receive the output of makeCacheMatrix for a specific matrix, and 
## check if a inverse has been cached. If so it will be displayed, othewise
## it will compute the inverse and cache it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

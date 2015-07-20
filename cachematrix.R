## makes Cache matix defines a vector of functions to 
## store and retrieve the inverse of a matrix 

## vector of functions to set  value of matrix m, get the value of m 
##  set and get the value of im ( inverse of m)

makeCacheMatrix <- function(m = matrix()) {
  m <- NULL
  set <- function(y) {
    m <<- y
    im <<- NULL
  }
  get <- function() m
  setinv <- function(m) im <<- solve(m)
  getinv <- function() im
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## returns the value of the inverse if cached in parent environment 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m 
  
}
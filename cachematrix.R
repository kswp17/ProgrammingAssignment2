##See each line for a description of what it does

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##initializes blank matrix
  set <- function(y = matrix()) {
    x <<- y ##sets value of the matrix to y in the parent environment
    i <<- NULL ##clears prior cache
  }
  get <- function() x ##retrieves matrix "x" from parent environment
  setinv <- function(solve) i <<- solve ##makes a function setinv that sets i as the output of solve in the parent environment
  getinv <- function() i ##retrieves i from the parent environment
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ##creates list of the created functions and names them so we can subset them with "$"
}

cacheSolve <- function(x, ...) {
  i <- x$getinv() ##sets i as the result of getinv from makeCacheMatrix function
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } ##if the inverse has already been calculated and i is not null, return inverse
  data <- x$get() ##else calculate inverse--set the result of get (x) above as the object "data"
  i <- solve(data, ...) ##set i as the inverse of "data"
  x$setinv(i) ##use the function setinv with argument i
  i ##print i
}
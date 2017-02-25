
##This function creates a cache matrix
makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {
  ## inv sets the value of the matrix
    x <<- y
    inv <<- NULL
    ## f(y) will find inverse of f(x)
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # if the inverse is calculated and not null, value is retreived from cache
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # else, calculate the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

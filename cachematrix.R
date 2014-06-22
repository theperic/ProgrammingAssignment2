## To save processing time this script creates an object that stores the cache of the inverse of a matrix.
## There are two functions: 
## 1) makeCacheMatrixfunctions to create a object with the matrix and its inverse and 
## 2) cacheSolve to efficiently retreive or recalculate the inverse as changes occur to the matrix.

## makeCacheMatrix creates the oject with teh matrix and the inverse for future use
## Write a short comment describing this function
  
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
    }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
  }
  
  ## This funciton first checks if the inverse has been set.  if so it returns it.
  ##  if not, it calculates it, caches it, then returns it.
    
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
  
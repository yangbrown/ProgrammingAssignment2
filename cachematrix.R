## Put a matrix into cache, then use the cached matrix to get a reverse matrix

## create Cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)        
  
}


## return inverse Matrix from cached data

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv        
  
}

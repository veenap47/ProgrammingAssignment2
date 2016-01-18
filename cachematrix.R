## This program can cache the inverse of a matrix so that when we need to find the 
## inverse of a matrix that has already been found, he computation is not
## redone and instead the result is read from the cache 


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv = NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix function for a new matrix and it fetches result from the 
## cache if the inverse of matrix has already been calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("geting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}

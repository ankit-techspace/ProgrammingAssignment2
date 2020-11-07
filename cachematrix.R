## Put comments here that give an overall description of what your
## functions do

## function to create a matrix object that can cache it's special "matrix"

makeCacheMatrix <- function(x = matrix()) {

  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## function to compute the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  if(!is.null(a)){
    message("Receiving cached data")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}

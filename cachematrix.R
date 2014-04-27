## Put comments here that give an overall description of what your
## functions do
## a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse value
  inverse <- NULL
  
  # set the value of matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get the value of the matirx
  get <- function() x
  
  # set the inverse
  setinverse <- function(inv) inverse <<- inv
  
  # get the inverse
  getinverse <- function() inverse
  
  # return a list of all the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # check if the inverse is cached.
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  # if not be cached, get the matrix
  data <- x$get()
  
  # compute the inverse using solve()
  inverse <- solve(data,...)
  
  # cache the inverse using setinverse function
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}
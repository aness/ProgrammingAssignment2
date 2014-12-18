## Put comments here that give an overall description of what your
## functions do

## BELHAOUARI ANESS,      PROGRAMMING ASSIGNMENT2



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  ## the inverse matrix of x will be cached in inv
  inv <- NULL
  ## the set matrix function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## the get matrix function
  get <- function() x
  ## the function to set the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  ## the functioon to get the inverse matrix
  getinverse <- function() inv
  ## list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## checking if the inverse is already calculated
  ## getting the inv from the cache
  inv <- x$getinverse()
  ## if it is not null ==> it is calculated for a matrix that have an inverse 
  if(!is.null(inv)) {
    ## just return the cache without calculus
    message("getting cached data.")
    return(inv)
  }
  ## else retrieve the cache 
  data <- x$get()
  ## calculate the inverse for a square invertible matrix
  inv <- solve(data)
  ## set the cache of x to the result so that we wont do calculations again
  x$setinverse(inv)
  ## show the solution
  inv
}

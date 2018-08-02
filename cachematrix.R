## Put comments here that give an overall description of what your
## functions do

## Overall objective of these two functions is to cache the inverse of a matrix
## No need to compute repeatedly after the caching is done

## Write a short comment describing this function
## Creates a list to set the value of matrix, get the value of matrix,
##  set the value of inverse of a matrix, get the value of inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## Returns the inverse of a matrix
## Checks whether inverse has already been computed
## If the cache has already calculated inverse, it directly prints it
## If not it computes the inverse, sets the value in cache via setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

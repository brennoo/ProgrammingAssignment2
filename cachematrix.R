## Programming Assignment 2 for R Programming on Coursera


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL            # variable that will receive the inverse matrix
  
  set <- function(y) {   # this function update the matrix value
    x <<- y
    inv <<- NULL         # re-seting the inverse due to a matrix change
  }
  
  get <- function() x                 # this function returns the current matrix value

  setinverse <- function(c) inv <<- c # this function stores the inverse matrix
  getinverse <- function() inv        # this function returns the current matrix value
  
  list(set = set, get = get,          # return a list() with all functions - they act as "methods" of the matrix
       setinverse = setinverse,
       getinverse = getinverse
       )
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()              # try to fetch the inverse matrix from cache
  if(!is.null(i)) {                # if cached value is not NULL
    message("getting cached data") # print that we are using cache data
    return(i)                      # returns the inverse matrix
  }

  # if the inverse matrix is not cached yet...
  data <- x$get()        # get the current matrix value
  i <- solve(data,...)   # compute the inverse
  x$setinverse(i)        # store the inverse value into the matrix cache
  i                      # returns the inverse matrix
  
}

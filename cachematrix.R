## coded by Derek J, June 2015; for R Programming class on Coursera
## programming assignment 2

# The 'makeCacheMatrix' function creates a special matrix object that can store
# its inverse in a cache

makeCacheMatrix <- function(x = matrix()) {
  # When the object is initialized, x is stored as a formal argument
  #
  # The following command initializes 'minv', which is the value of the cache
  # for the matrix's inverse, and sets the value to 'NULL':
  minv <- NULL
  # The 'set' function stores a new input matrix in the return object and sets
  # the value of the cache to 'NULL' (since the old minv value is no longer valid)
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  # The 'get' function simply returns the value of the matrix stored in the
  # object
  get <- function(){
    x
  }
  # The 'setinv' function sets the value of the cache to the input argument
  setinv <- function(z){
    minv <<- z
  }
  # The 'getinv' function simply returns the value of the cache
  getinv <- function(){
    minv
  }
  # The following command returns all four functions in a list format.  The values
  # of 'x' (the matrix), and 'minv' (the cached value of the x's inverse) are stored
  # in the 'makeCacheMatrix' function's environment (i.e., the function closure for
  # 'makeCacheMatrix')
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # The 'cacheSolve' function returns the inverse of a matrix stored in the
    # special object created using the 'makeCacheMatrix' function. If the
    # object passed as the input argument already has a value stored in the
    # cache, it will return the cached value.  If not, 'cacheSolve' will
    # compute the inverse, set the cache value to the computed result, and
    # return it as the function output.
    if(!is.null(x$getinv())){
        print('retrieving cache value')
        return(x$getinv())
    }
    calcinv <- solve(x$get())
    x$setinv(calcinv)
    calcinv
}

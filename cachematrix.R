# coded by Derek J, June 2015; for R Programming class on Coursera
# programming assignment 2

# The 'makeCacheMatrix' function creates a special matrix object that can store
# its inverse in a cache.

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


# The 'cacheSolve' function returns the inverse of a matrix stored in the special
# object created using the 'makeCacheMatrix' function.  If the object passed as the
# input argument already has a value stored in the cache, it will return the cached
# value.  If not, 'cacheSolve' will compute the inverse, set the cache value to the
# computed result, and return it as the function output.

cacheSolve <- function(x, ...) {
    # The following conditional execution determines if there is already a value
    # stored in the cache of the input object.  If so, execution ends and the
    # existing cache value is returned:
    if(!is.null(x$getinv())){
        print('retrieving cached value')
        return(x$getinv())
    }
    # The following code calculates the inverse of the matrix stored in the input
    # argument's environment, and stores it in a free variable called 'calcinv':
    calcinv <- solve(x$get())
    # The following command sets the cache value in the argument object:
    x$setinv(calcinv)
    # The following command returns the computed inverse matrix as the function output:
    calcinv
}

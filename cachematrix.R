## This function computes the inverse of the special "matrix" returned by function makeCacheMatrix. 
## f the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve must retrieves the inverse from the cache.

## This behaviour can help to improve performance if the inverse of a matrix has to be calculated 
## repetitively (e.g. in a for loop)

## The "matrix" object is a list containing a function to:

## set the value of the "matrix" object
## get the value of the "matrix" object
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL                                       # initialize the inverse cached matrix to NULL
  set  <- function(y){                             # set the new value for the matrix
    x <<- y                                        # superassign value
    i <<- NULL                                     # reset cached inverse matrix to NULL
  }
  get  <- function() x                             # function to obtain the value of matrix
  setinverse  <- function(inverse) i  <<- inverse  # function to set the inverse matrix
  getinverse  <- function() i                      # function to get the inverse matrix
  list(set= set, get = get,                        # return a list of all the functions
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

## cacheSolve: This function computes the inverse of the special "matrix" object returned 
## by makeCacheMatrix function above. 

## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {                   
  i  <- x$getinverse()                             # get the cached inverse matrix
  if (!is.null(i)){                                # check if the inverse is cached, so if it is different to NULL
    message("getting cached data")
    return(i)
  }
  data  <- x$get()                                 # if not be cached (so if it is NULL), calculate the inverse of the matrix
  # size <- nrow(data)
  # i <- solve(data,diag(1,size,size))
  i  <- solve(data, ...)   # %*% data              # compute the inverse using solve()
  x$setinverse(i)                                  # cache the inverse using setinverse function
  i                                                # return a matrix that is the inverse of 'x'
}

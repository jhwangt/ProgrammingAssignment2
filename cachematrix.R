## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

# The first function, `makeVector` creates a special "vector", which is
# really a list containing a function to
# 
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# The following function calculates the mean of the special "vector"
# created with the above function. However, it first checks to see if the
# mean has already been calculated. If so, it `get`s the mean from the
# cache and skips the computation. Otherwise, it calculates the mean of
# the data and sets the value of the mean in the cache via the `setmean`
# function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
# The function, `makeVector,' creates a special "matrix" object, which is
# really a list containing a function to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invX<-NULL
  set <- function(y){
    x<<-y
    invX<<-NULL
  }

  get <- function() x
  setInverse <- function(inv) invX <<- inv
  getInverse <- function() invX
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has
# not changed), then the cacheSolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  invX <- x$getInverse()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  dataMx <- x$get()
  invX <- 1/dataMx
  x$setInverse(invX)
  ## Return a matrix that is the inverse of 'x'
  invX
}
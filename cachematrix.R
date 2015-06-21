# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(input_matrix = matrix()) {
  inv <- NULL
  set <- function(set_var) {
    input_matrix <<- set_var
    inv <<- NULL
  }
  get <- function() input_matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(input_matrix, ...) {
  inv <- input_matrix$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- input_matrix$get()
  inv <- solve(data)
  input_matrix$setinverse(inv)
  inv
}

# Sample run:
# > x = rbind(c(1, -1/6), c(-1/6, 1))
# > m = makeCacheMatrix(x)
# > m$get()
#[,1]       [,2]
#[1,]  1.0000000 -0.1666667
#[2,] -0.1666667  1.0000000


# No cache at first
# > cacheSolve(m)
#[,1]      [,2]
#[1,] 1.0285714 0.1714286
#[2,] 0.1714286 1.0285714


# Retrieving from the cache afterwards
#> cacheSolve(m)
#getting cached data.
#[,1]      [,2]
#[1,] 1.0285714 0.1714286
#[2,] 0.1714286 1.0285714

 
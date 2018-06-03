## The assignment is to write a pair of functions that cache the inverse of a matrix.
## Created by Ellen Pols

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
## set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
## get the value of the matrix
  get <- function() x
## set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
## get the value of the inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    i <- x$getinverse() 
    if (!is.null(i)) { 
      message("getting cached data")
      return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    
  }

#TEST (including the results from the console)

# > A <- matrix(c(2,4,6,8),2,2)
# > A1 <- makeCacheMatrix(A)
# > cacheSolve(A1)
#      [,1]  [,2]
# [1,] -1.0  0.75
# [2,]  0.5 -0.25
# > A <- makeCacheMatrix(matrix(1:4,2,2))
# > A$get() 
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(A)
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > A$getinv()
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
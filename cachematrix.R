## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makecacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#- If the inverse has already been calculated (and the matrix has not changed),
#- then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
# makeCacheMatrix creates a object with the matrix and the 4 functions associated with it 
# set = to assign a value to the current matrix
# get = get the value of the matrix 
# setinverse = to compute the inverse of the matrix
# getinverse = to compute the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve check if the inverse of the matrix is already available. 
# If yes, it passes the available inverse as is. It calculated the inverse only incase it is not available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 matrix_inv <- x$getinv() 
  if(!is.null(matrix_inv)) {
    message("getting cached data")
    return(matrix_inv)
  } 
  else {
    data <- x$get()
    matrix_inv <- solve(data)
    x$setinverse(matrix_inv)
    #return(matrix_inv)
  }
}

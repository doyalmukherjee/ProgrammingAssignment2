#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix,
#rather than compute it repeatedly
#These functions below are used to create an object that stores a matrix,
#and also used to cache the inverse of the matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matrix_inv_data <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv_data <<- NULL #sets value to an object in an environment that is different from the current environment 
  }
  get <- function() x
  setInverseMatrix <- function(solve) matrix_inv_data <<- solve
  getInversematrix <- function() matrix_inv_data
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInversematrix = getInversematrix)
  
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #Returning the inverse of matrix 'x'
  matrix_inv_data <- x$getInversematrix()
  
  if(!is.null(matrix_inv_data)) { #Check cached data
    message("Getting cached data......")
    return(matrix_inv_data)
  }
  data <- x$get()
  matrix_inv_data <- solve(data, ...)
  x$setInverseMatrix(matrix_inv_data) #set data back in cache
  matrix_inv_data
}

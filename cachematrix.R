## makeCacheMatrix creates a special matrix object which
## sets the value of the matrix
## gets the value of the matrix
## sets the inverse of matrix
## gets the inverse of matrix

makeCacheMatrix <- function(mat = matrix()) {
  inv_mat <- NULL
  ## sets a new matrix
  set <- function(new_mat) {
    mat <<- new_mat
    inv_mat <<- NULL
  }
  ## gets the matrix
  get <- function() mat
  ## sets the inverse of matrix
  setinverse<- function(inverse) inv_mat <<-inverse
  ## gets the inverse of matrix
  getinverse <- function() inv_mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve creates and caches the inverse of a matrix
## if the matrix has already been cached it returns the cached value
## or else it creates the inverse of matrix again, caches it and returns it.
cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'mat'
  inv_mat <- mat$getinverse()
  if (!is.null(inv_mat)) {
    message("getting cached inverse matrix")
    return(inv_mat)
  } else {
    message("creating inverse matrix")
    inv_mat <- solve(mat$get())
    mat$setinverse(inv_mat)
    return(inv_mat)
  }
}
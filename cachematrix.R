## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {              # takes a matrix-type object as an argument
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve             
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cacheSolve should retrieve the inverse from the cache.
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}                                                  

## will give error if non-square matrix
## will give error if matrix is singular (non-invertible, determinant is 0, ad-bc=0)

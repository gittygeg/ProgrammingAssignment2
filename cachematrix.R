# # makeCacheMatrix accepts an invertible matrix, creates an 
# inverted matrix and will output either matrix to the console
# # cacheSolve accepts an invertible matrix and will return 
# an inverted matrix from first calculation or cache for subsequent
# invocations with the unchanged starter matrix

# these methods set and get a matrix value
# setinverse will create and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# m is set to the inverse of a matrix in x, if it exists
# otherwise, the matrix is calculated, set in x, and returned
# to x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m  
}

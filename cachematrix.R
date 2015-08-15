# Stores the contents of a matrix and its inverse
# such that when the contents of the matrix remain unchanged, 
# the inverse of the matrix is cached so that when we need it again, 
# it can be looked up in the cache rather than recomputed. 

# Creates a special "matrix"
# which is actually a list of functions to
# 1.	set the value of the matrix (set)
# 2.	get the value of the matrix (get)
# 3.	set the inverse of the matrix (setinverse)
# 4.	get the inverse of the matrix (getinverse)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get= get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


# Gets the inverse of the special 'matrix' created with makeCacheMatrix
# if the inverse has already been calculated, returns the cached value
# otherwise calculates the inverse of the matrix using the solve function
# and stores it in the cache for the special 'matrix'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}

## The set of functions together calculate the inverse of a matrix. As inverses can take
## a lot of computation, we create a cache of the inverse and use it if necessary

## makecachematrix creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initializes the matrix as null to be used later
  i <- NULL
  ## function to create the matrix object in an environment other than current one
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##function to get the matrix input
  get <- function() x
  
  ## function to set the inverse of the matrix 
  setinverse <- function(solve) i <<- solve
  ##function to get the inverse of the matrix
  getinverse <- function() i
  
  ##declaration of functions to be used later
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cachesolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ##check if the inverse exists in cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ##get the matrix object
  data <- x$get()
  
  ##calculate the inverse
  i <- solve(data)
  ##set the inverse object 
  x$setinverse(i)
  i
}

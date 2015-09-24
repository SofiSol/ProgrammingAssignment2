## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x 
  
  ## define the cache 
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x 
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal
  ## to the inverse of the matrix x
  getinverse <- function() m ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {# check to see if tthe inverse matrix has been calculated
    message("getting cached data")
    return(m)
  }
  data <- x$get()#get the inverse of the matrix if it hasn't been calculated and caches it
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


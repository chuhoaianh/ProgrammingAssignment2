#This function creates a special "matrix" object that can cache its inverse.
#It takes a square matrix, i.e x <- matrix(c(11,22,53,34,35,56,57,88,69), nrow = 3, ncol = 3)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    print("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

#Example
#x <- matrix(c(11,22,53,34,35,56,57,88,69), nrow = 3, ncol = 3)
#m <- makeCacheMatrix(x)
#cacheSolve(m)

#             [,1]        [,2]        [,3]
# [1,] -0.05736133  0.01931066  0.02275736
# [2,]  0.07181009 -0.05163205  0.00652819
# [3,] -0.01422050  0.02707144 -0.00828578
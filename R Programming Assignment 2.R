# 1.makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <- y
        inv <- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) inv <- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# 2.cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

# Testing my function
mdum <- matrix(1:4,2,2)
mdum
mdum1 <- makeCacheMatrix(m)
mdum2 <- cacheSolve(m1)
mdum2

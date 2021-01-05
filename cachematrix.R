## Make two functions (makecachematrix and cache solve)
## that will be able to cache the inverse of a matrix

## makeCachematrix is Creating a Cache matrix 
## that will be able to cache its inverse
makeCacheMatrix <- function(x = matrix()){
  ##This will provide the inverse property
  inv <- NULL
  ## The first step is setting the value of the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## The second step is obtaining the value of the matrix
  get <- function() {x}
  
  ## The third step is setting the value of inverse matrix
  setInverse <- function(inverse) {inv <<- inverse}
  
  ## The fourth step is obtaining the value of the inverse matrix
  getInverse <- function() {inv}
  
  ##Make a list for the function
  list(set = set, 
       get = get,
       setInverse=setInverse, 
       getInverse = getInverse)
}
##cachesolve, it calculates the inverse of the cache matrix
##that was made by the first function
cacheSolve <- function(x, ...){
  
  ##Return a matrix that is invers of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("processing cached data")
    return(inv)
  }
  
  ##obtain the matrix from the object
  mat <- x$get()
  
  ## Generate the inverse matrix
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

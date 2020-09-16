## These set of functions create a list of functions for creating a matrix vectorand setting the matrix and the inverse
## If the matrix inverse is not cacehd in (first function / second function previously) then
##      second function calculates the matrix and caches it
## If the matrix inverse is cached (first function / second function previously) then 
##      second function displays the cached inverse


## Creates a list of functions to set the original matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Returns the matrix inverse 
##   If inverse is cached then returns from cache else calculates the inverse, caches it and displays

cacheSolve <- function(x, ...) {
  
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  matrix <- x$get()
  inverseMatrix <- solve(matrix, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
        ## Return a matrix that is the inverse of 'x'
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                  inv <- NULL 
                  ## set the value of the matrix
                  set <- function(y){
                    x <<- y
                    inv <<- NULL
                  }
                  ## get the value of the matrix
                  get <- function()x
                  ## set the value of the inverse
                  setInverse <- function(inverse) inv <<- inverse
                  ## get the value of the inverse
                  getInverse <- function()inv
                  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' and assign to inv
        inv <- x$getInverse()
        ## if the inverse has already been calculated
        if(!is.null(inv)){
          ##get from the cache and skip the calculation and display a message
          message("getting cached data")
          return(inv)
        }
        
        ##if not, calculate the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        
        ##sets the value of the inverse in the cache
        x$setInverse(inv)
        inv
}

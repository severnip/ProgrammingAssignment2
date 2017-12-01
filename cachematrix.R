## This function caches an inverse value of a square matrix. Then,
## can retrieve the cached value of the matrix at a later time.

## This first function creates a list of function that set and get
## the values of both the matrix and it's inverse.'

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m<<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function()m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This second function can identify whether a matrix's
## inverse is cached. If the inverse is cached, it will 
## return the cached value. If not, it will calculate the 
## inverse of the matrix.

cacheSolve <- function(x, ...) {
       m <- x$getInverse()
       if(!is.null(m)){
         message("Getting cached data")
         return(m)
       }
   ## Return a matrix that is the inverse of 'x'
       data <- x$get()
       m <- solve(data, ...)
       x$setInverse
}

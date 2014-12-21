## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Create invM variable
    invM <- NULL
    ## Set the value of the matrix
    set <- function (y){
        x <<- y
        invM <<- NULL
    }
    ## Get the value of the matrix
    get <- function() x
    ## Set the value of the inverted matrix
    setInv <- function(inverse) invM<<- inverse
    ## Get the value of the inverted matrix
    getInv <- function() invM
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get the value of the inverted matrix into "invM" 
    invM <- x$getInv()
    ## If the matrix has been inverted return it
    if(!is.null(invM)){
        message("getting cached data")
        return(invM)
    }
    ## Otherwise get the original matrix, 
    data <- x$get()
    ## invert it
    invM <- solve(data, ...)
    ## and cash it.
    x$setInv(invM)
    ## Finally return the inverted matrix
    invM
}

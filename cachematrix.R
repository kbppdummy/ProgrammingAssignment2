## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    cached <- NULL
    
    set <- function(y){
        x <<- y
        cached <<- NULL
    }
    
    get <- function(){ x }
    setinv <- function(inv){ cached <<- inv }
    getinv <- function(){ cached }
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    cached <- x$getinv()
    
    if(!is.null(cached)){
        message("Inverse already computed.")
        message("Getting cached data...")
        return(cached)
    }
    
    data <- x$get()
    cached <- solve(data)
    x$setinv(cached)
    
    cached
    
}

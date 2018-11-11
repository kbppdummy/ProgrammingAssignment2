##
##   The document contains two(2) functions:
##       1. makeCacheMatrix 
##          This returns a list containing a matrix (empty or from user) and
##              some other functions that can be used to get cached inverse
##              data for the set matrix (or solve it, if cache is empty).
##       2. cacheSolve
##          This function checks first if the matrix for the list has been
##              changed. If it has not been changed, then it will return
##              the previously computed inverse for the matrix (cached).
##              Else, it will compute for the inverse of the new matrix.
##
##   Usage:
##      1. Create a cachematrix first using makeCacheMatrix. You can supply
##              your matrix. It defaults to an empty matrix.
##      2. Use cacheSolve to compute for the inverse of the matrix in
##              your previously created cachematrix.
##
##   Example usage:
##      cMatrix <- makeCacheMatrix(x)   #creating the cache matrix
##                                      #assume x is a matrix
##      cacheSolve(cMatrix)             #this will not compute for the inverse
##                                      #of x (the matrix given to cMatrix)
##

## FUNCTION DEFINITIONS ########################################################

## creates a cacheMatrix (a list containing the matrix and the functions that
##     can be applied to the given matrix)
makeCacheMatrix <- function(x = matrix()) {

    #initialize cached as empty (NULL)
    cached <- NULL
    
    #define function set for setting the matrix to compute
    set <- function(y){
        x <<- y
        cached <<- NULL
    }
    
    #function getting the matrix being computed
    get <- function(){ x }
    
    #function sor getting the cached inverse
    setinv <- function(inv){ cached <<- inv }
    
    #function for getting the cached inverse
    getinv <- function(){ cached }
    
    #returning the cacheMatrix
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## if the matrix did not change, this will return the cached value
##     else, this will compute for the inverse of the matrix in the x parameter
cacheSolve <- function(x, ...) {
    
    #gets the cached inverse
    cached <- x$getinv()
    
    #if cache is not empty (cache is not NULL), return cached value
    if(!is.null(cached)){
        message("Inverse already computed.")
        message("Getting cached data...")
        return(cached)
    }
    
    #else, the matrix is retrieved from x
    data <- x$get()
    
    #the inverse of x is computed and given to cached variable
    cached <- solve(data)
    
    #the computed inverse is set to the cached variable of the cacheMatrix
    x$setinv(cached)
    
    #the computed value is returned
    cached
    
}

## END OF FILE
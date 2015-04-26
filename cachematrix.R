## Together these functions establish an object that can store matrix along with its
## inverse. The inverse is not set until needed, then saved as a cache that can be
## accessed again without needing to resolve the inverse. 


## This function establishes the cache object with methods to store/retrieve the matrix
## and its inverse. The function returns a list that contains for methods for saving 
## and retrieving the information.

makeCacheMatrix <- function(mat = matrix()) {
    ## setting object to store the inverse
    inv <- NULL
    
    ## method to input a new matrix (and delete previously calculated inverse)
    set <- function(newmat){
        mat <<- newmat
        inv <<- NULL
    }
    
    ## method to retrieve matrix
    get <- function() mat
    
    ## method for storing inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## method for retrieving inverse (will be NULL if inverse not previously calculated and store)
    getinverse <- function() inv
    
    ## returning a list object with methods for storing and retrieving matrix and cached inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Controlling function that calls the inverse from the CacheMatrix item. If the inverse is already
## calculated and cached, the function returns the inverse. If the inverse is not cached, the function
## calcuates the inverse, stores it in cache, and returns it.

cacheSolve <- function(cachemat, ...) {
    ## retrieve cached inversed
    inv <- cachemat$getinverse()

    ## if inverse already calced and cached, return it
    if (!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    
    ## only get here if condition fails (i.e., cached inverse is null)
    message("calculating inverse")
    
    #calculate inverse
    inv <- solve(cachemat$get(),...)

    ## store inverse solution
    cachemat$setinverse(inv)
    
    ## return inverse solution
    inv
}

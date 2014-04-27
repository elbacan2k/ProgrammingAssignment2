## Put comments here that give an overall description of what your
## functions do

## this function create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## set m initially to null
    m <- NULL 
    ## set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get function
    get <- function() { x }
    ## set inverse
    setinverse <- function(inv){ m <<- inv }
    ## get inverse
    getinverse <- function() { m }
    ## set list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## this function computes the inverse of the special matrix returned by makeCacheMatrix
## if inverse has already been calculated and has not changed, 
## then retrieve the inverse from the cache  
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## set m by getting current inverse
    m <- x$getinverse()
    
    ## if m is null then print message and return m which will print NULL
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## set data to curent x matrix
    data <- x$get()
    ## set m to be the inverse of data matrix
    m <- solve(data)
    ## set cache inverse
    x$setinverse(m)
    ## return m
    m    
}

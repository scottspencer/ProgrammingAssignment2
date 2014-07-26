## Put comments here that give an overall description of what your
## functions do

## This function is essentially a constructor function to create a matrix-like object, complete with functions
## which can be used to set and retrieve the matrix itself, as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }  ## creates set() function for the matrix object, which allows for (re)setting the value of the matrix
    get <- function() x  ## creates get() function for the matrix object, which returns the matrix
    setinverse <- function(solve) m <<- solve  ## creates setinverse() function for the matrix object, which creates a 
                                                ## cached value of the inverse of the matrix object
    getinverse <- function() m  ## creates setinverse() function for the matrix object, which returns the 
                                ## cached value of the inverse of the matrix object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function caches the value of a passed matrix-like object (created using makeCacheMatrix), and returns it.
## In future calls, it announces that it's saving the cached value of the inverse, rather than recalculating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()  ## sets the value of "m" to the cached inverse of the matrix-like object passed
    if(!is.null(m)) {  ## if that inverse value has not already been cached, this results in "m" being null
        message("getting cached data")  # if m is not null, then it announces that it's returning the cached version
        return(m)  ## and then returns it
    }
    data <- x$get()  ## if "m" is null, then it uses the get() function to create a copy of the matrix in the function environment
    m <- solve(data, ...) ## then it uses the solve() function to get the inverse of the matrix, and sets "m" to that value
    x$setinverse(m) ## then uses setinverse() to cache that calculated inverse for later use
    m ## and finally, returns the newly calculated inverse
}
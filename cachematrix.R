## Functions for creating matrices with the ability to keep its inverse in memory.

## Functions usage example:
## a<-matrix(c(2,3,2,1,2,1,1,1,2),nrow=3,ncol=3)
## a1<-makeCacheMatrix(a)
## cacheSolve(a1) # solve() will be called and the inverse matrix result will be stored in memory
## cacheSolve(a1) # a message will show that it is obtaining the inverse matrix from memory

## Description: makeCacheMatrix takes in a matrix input, saves the matrix then output the matrix as a 
##              functions list with set,get,setinverse,getinverse.
##
##              set: allows changing of the original input matrix
##              get: obtain the original saved matrix
##              setinverse: allows setting of the inverse of the input matrix regardless of the original
##              getinverse: obtain the saved inverse matrix

makeCacheMatrix <- function(input = matrix()) {
    # Create empty object
    inverse <- NULL 
    
    # Create set function to store input variable into the current environment
    set <- function(inverse) {
        input <<- inverse
        inverse <<- NULL
    }
    # Returns input matrix
    get <- function() input
    # Stores input_inverse variable into the current environment
    setinverse <- function(input_inverse) inverse <<- input_inverse
    # Returns inverse matrix if it was previously saved or set
    getinverse <- function() inverse
    
    # Create the list of functions and return it
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Description: cacheSolve takes in a cache matrix list that was created by makeCacheMatrix and output
##              the inverse matrix if it was calculated previously. If not, it will calculate, save 
##              then output the inverse matrix of the matrix that was passed into makeCacheMatrix.

cacheSolve <- function(input_cache_matrix, ...) {
    ## Return a matrix that is the inverse of 'input_cache_matrix' if it is previously cached
    inverse_matrix <- input_cache_matrix$getinverse()    
    if(!is.null(inverse_matrix)) {
        message("Getting cached inverse matrix")
        return(inverse_matrix)
    }
    ## Perform solve() and save the result if it was not previously cached
    data <- input_cache_matrix$get()
    inverse_matrix <- solve(data, ...)
    input_cache_matrix$setinverse(inverse_matrix)
    
    ## Return the result
    inverse_matrix
}

## The two functions in this code help to reduce the computational load of complicated
## calculations involving repeating data. This is done by caching a value and looking
## at it again instead of recomputing. In this particular case the inverse of a matrix
## is being calculated by the function cacheSolve while makeCacheMatrix is used to cache 
## the inverse.



## makeCacheMatrix is a function that stores a list of functions such as get, set,
## setinv_mat & getinv_mat. We call this function first by passing the matrix we want
## to find the inverse for as argument. More details parallel to code line.

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL                  # inv_mat is set to null, useful for first call of data                     
        set <- function(y) {             # set function changes the matrix stored in main
                x <<- y                  # Substitutes x with input y in main function(not just local)
                inv_mat <<- NULL         # old inverse is not needed since new matrix is going to be solved
                
        }
        get <- function() x              # returns the matrix x store in main function
        setinv_mat <- function(inv) {
                inv_mat <<- inv          # stores the input in the matrix inv_mat (not just local)
        }
        getinv_mat <- function() inv_mat # returns the inv_mat matrix to main function 
        list(set = set ,get = get,       # creates a list of functions
             setinv_mat = setinv_mat,
             getinv_mat = getinv_mat)
}


## cacheSolve shows the inverse of a matrix either by retrieving from cache or by computing 
## using the solve function. Input is object where makeCacheMatrix is stored. For example if
## inverse of (2, 2, 3, 2) is to be found, we first call makeCacheMatrix, a <- makeCacheMatrix(c(2,2,3,2))
## then get the inverse by calling cacheSolve, inv <- cacheSolve(a)

cacheSolve <- function(x, ...) {
        
        inv_mat <- x$getinv_mat()              # Get the cached object
        if(!is.null(inv_mat)) {                # Check is cache exists, if yes then print                            
                message("Getting cached data") # message and return the inverse
                return(inv_mat)
        }
        dat <- x$get()                         # if its new data use the solve function to
        inv_mat <- solve(dat, ...)             # to compute the inverse of the matrix
        x$setinv_mat(inv_mat)                  # pass on the inverse to cache for future use
        inv_mat                                # Display the inverse matrix
}
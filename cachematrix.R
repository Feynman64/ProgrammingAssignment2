## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## USAGE :
##
## for a given matrix m :
## ---- create a cached environment for m ----
## m_cache <- makeCacheMatrix(m)
## ---- proceed with calculation using this cached environment ---
## cacheSolve(m_cache)

## this function return a list of functions to set a matrix
## get a matrix, set the inverse of the matrix and get its
## inverse
## it isolates an environment to store results

makeCacheMatrix <- function(x = matrix()) {

        ## initialize matrix m which will contain the inverse
        m <- NULL
          
        ## function to set the matrix
        set <- function(p) {
                x <<- p
                m <<- NULL
        }
        
        ## function to get the matrix
        get <- function() x
        
        ## function to set the inverse
        setinv <- function(solve) m <<- solve
        
        ## function to get the inverse if it already exists
        getinv <- function() m
        
        ## return the list of function to access this workign environment.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        }


## Write a short comment describing this function

## This function will calcultate the inverse of a matrix unless it has
## already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check if x is a list
        if (typeof(x) == "list") {
                
                ## get the inverse value from makeCacheMatrix
                m <- x$getinv()
        
                ## if the inverse exists, return the inverse
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                
                ## Else procede with the calculation
                data <- x$get()
                
                ## calculate the inverse
                m <- solve(data, ...)
                
                ## save the result
                x$setinv(m)
                
                ## return the result (m is the inverse of x)
                m
        } else {
                ## if not a list, return a message
                message("")
                message("This function must be called with a list")
                message("First, call makeCacheMatrix")
                message("example :")
                message("  > m_cache <- makeCacheMatrix(m)")
                message("  > cacheSolve(m_cache)")
                message("")
        }
}
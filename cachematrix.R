#######################################################
#  2014-10-26 TSG
#
#  Description
#
#     these functions cache the computation of the inverse of 
#      a matrix and return the saved/cached value for the
#      second and subsequent calls if the input has not
#      changed
#
#  Usage 
#     
#    1. source this file  
#    2. <variable, e.g., m > <- makeCacheMatrix(<input matrix> OR () )       
#         if no input provided you must subsequenetly run m$set(<input matrix>)
#    3. cacheSolve(m) 
#          first and subsequent calls will return inverse matrix but subsequent calls with 
#          same input will use cached value and print out a "getting cached data" message 
#
#  Assumptions 
#  
#     1.  input matrix is always invertible 
#     2. no caching will be done for null/empty matrix
#
#######################################################

makeCacheMatrix <- function(x = matrix()) {

# constructor function for cached inverse matrix calculation
# containing functions = set, get, setimatrix, getimatrix

        im <- matrix()
        set <- function(y) {
                x <<- y
                im <<- matrix()
        }
        get <- function() x
        setimatrix <- function(imatrix) im <<- imatrix
        getimatrix <- function() im
        list(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
}


cacheSolve <- function(x, ...) {

# function to return computed or cached inverse matrix  
# input is output of makeCacheMatrix function

        im <- x$getimatrix()
        if(!is.na(im[1,1])) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setimatrix(im)
        im

}

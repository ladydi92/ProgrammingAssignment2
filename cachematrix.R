## These functions cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix"  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 		reversed_matrix <- NULL
        	set <- function(y) {
                x <<- y
                reversed_matrix <<- NULL
        }
        get <- function() x
        set_matrix <- function(solve) reversed_matrix <<- solve
        get_matrix<- function() reversed_matrix
        list(set = set, get = get,
             set_matrix = set_matrix,
             get_matrix = get_matrix)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix  

cacheSolve <- function(x, ...) {
       reversed_matrix <- x$get_matrix()
        if(!is.null(reversed_matrix)) {
                message("getting cached data")
                return(reversed_matrix)
        }
        data <- x$get()
        reversed_matrix<- solve(data, ...)
        x$set_matrix(reversed_matrix)
        reversed_matrix
} 

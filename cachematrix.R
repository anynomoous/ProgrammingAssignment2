## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. This is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(actual_matrix = matrix()) {
    
    ## cached inversed matrix
    inverse_matrix <- NULL
    
    ## getter and setter for the actual matrix
    set <- function(new_matrix) {
        actual_matrix <<- new_matrix
        ## The cached inverse is now invalid because the matrix has changed
        inverse_matrix <<- NULL
    }
    get <- function() {
        actual_matrix
    }
    
    ## getter and setter for the inverse matrix
    set_inverse <- function(calculated_inverse) {
        inverse_matrix <<- calculated_inverse
    }
    get_inverse <- function() {
        inverse_matrix
    }

    ## return the list of functions provided by makeCachedMatrix
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolvecomputes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(cache_matrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    cached_inverse <- cache_matrix$get_inverse()
    if(!is.null(cached_inverse)) {
        ## getting cached data
        return(cached_inverse)
    } else {
        ## calculating data and caching for next time
        data <- cache_matrix$get()
        inverse_matrix <- solve(data, ...)
        cache_matrix$set_inverse(inverse_matrix)
        return(inverse_matrix)
    }
}

## This combination of functions is intended to avoid repeatedly computing the inverse of the same matrix by cacheing the inverse.

## This first function produces a list containing four functions which, respectively, set the value of a matrix, retreive a matrix, 
## set the value of the inverse matrix, and retrieve the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse_mat <- function(inverted) m <<- inverted
        get_inverse_mat <- function() m
        list(set = set, get = get,
             set_inverse_mat = set_inverse_mat,
             get_inverse_mat = get_inverse_mat)
}


## When the above function and a matrix are passed to the following function, it will compute the inverse of the matrix, 
## but first it checks if that value is already cached to avoid unnecessary computation. 

cacheSolve <- function(x, ...) {
        m <- x$get_inverse_mat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse_mat(m)
        m
	  ## Return a matrix that is the inverse of 'x'
}

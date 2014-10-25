## The functions below cache the inversion of matrix.
## My functions works analogical as the function caching mean. 


## This function creates a matrix object. 
## It sets the s object (designed for the inverse of a matrix)
## to be a matrix with NA values. The size of the s is equal to the size
## of the original matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- matrix(NA, nrow(x), ncol(x))
        set <- function(y = matrix()) {
                x <<- y
                s <<- matrix(NA, nrow(y), ncol(y))
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## The function computes inverse of the matrix using the solve function. 
## It assignes the inversed matrix to the s object.
## If the value of s has been computed before it returns the cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.na(s[1,1])) {
              message("getting cached data")
              return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
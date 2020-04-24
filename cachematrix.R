## The first function creates a matrix and defines get and set functions and outputs a list of functions.
## The second function creates the inverse of a matrix if it is not already present in the cache.

## Creates matrix using set and get methods.
## Input i.e. x is a square matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(mat){
                x <<- mat 
                inv <- NULL 
        }
        
        get <- function(){x} 
        
        set_inverse <- function(inverse){
                inv <<- inverse
        }
        
        get_inverse <- function(){inv}
        
        list(get = get, set = set, set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Returns a matrix which is a inverse of x
## Input i.e. x is of the type makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data_matrix <- x$get()
        inv <- solve(data_matrix)
        x$set_inverse(inv)
        inv
}
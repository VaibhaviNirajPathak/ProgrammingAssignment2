
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 
#<<- operator is used to assign a value to an object in an environment that is different 
#from the current environment 


makeCacheMatrix <- function(x = matrix()) {  # define the argument with default mode of "matrix"
        inv <- NULL                         #assign NULL to inv variable
        set <- function(y) {                #define set function
                x <<- y                     #value of matrix in parent environment
                inv <<- NULL                #if there is a new matrix, reset inv to NULL
        }
        get <- function() x                                #get function defined and return value of matrix argument
        setInverse <- function(inverse) inv <<- inverse    #set the value of the invertible matrix
        getInverse <- function() inv                       #get value of invertible matrix
        list(set = set,                                     ## you need this in order to refer to the functions with the $ operator
             setInverse = setInverse,
             getInverse = getInverse)
}

 ## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()                            
        if (!is.null(inv)) {                        #if inverse matrix is not NULL
                message("getting cached data")      # print message getting cached data
                return(inv)                         #return the invertible matrix
        }
  #if value of the invertible matrix is NULL then  
        mat <- x$get()                              ##get the original Matrix Data 
        inv <- solve(mat, ...)                      #solve function to inverse the matrix
        x$setInverse(inv)                           #set the invertible matrix 
        inv                                         #return the invertible matrix
}

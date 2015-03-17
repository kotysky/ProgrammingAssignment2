## Put comments here that give an overall description of what your
## functions do
## There are two functions, makeCacheMatrix and cachesolve,and they try to use the lexical scoping 
## to store inverts of matrix in memory, and if its invers is already stored in memory, then they get the 
## invert stored from memory avoiding superflous calculations



## the first function, prepare an invertible matrix to stored or not
## in memory.
## This function returns a list with the functions to be used in 
## its enviroment.
## The list returned contains the functions you can apply over the matrix
## in its own enviroment, and this function just can be used over a 
## a variable assigned with the function makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL                       ## initalize m
        set <- function(y) {            ## Declares function "set" to be used to cache values
                x <<- y
                m <<- NULL
        }                       
        get <- function() x             ## Declares function "get" to get the matrix to be stored
                                        ## from its parent environment
        
        setinvert <- function(invert) m <<- invert ## Declares a function "setinvert"
                                                   ## to cache the  matrix in memory
        getinvert <- function() m       ## Declares a function "getinvert" to get the matrix from memory
        
        list(set = set, get = get,      ## Creates a list with the function to be used in this environment
                                        ## over de x variable
             setinvert = setinvert,
             getinvert = getinvert)

}


## This function, compare the variable to be used, with its enviroment
## and if it isnt cached in its enviroment(get$mean=null) 
## then the inverse is calculated and cached ,else,
## if the variable is already cached then 
## function just return the value stored. 

cacheSolve <- function(x, ...) {
        
        m <- x$getinvert()      ## takes from memory the value.
        
        if(!is.null(m)) {       ## if its invert exits stored in memory 
                                ## the it takes from memory and returns the value stored
                message("getting cached data")
                return(m)
        }
                                ## else its means there is not stored de invert in memory
        data <- x$get()         ## obtains the matrix from x
        m <- solve(data, ...)   ## Cacultes the matrix x invert
        x$setinvert(m)          # stores the invert in memory
        m                       ## returns the value of the matrix invert,
}

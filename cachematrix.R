## makeCacheMatrix
#This function is for constructing a matrix, displaying the data in matrix and for
#checking if the inverse of the function is computed(or stored in "inverse").

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse#seting the invers 
        getInverse <- function() j #serch for the invers 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

##cacheSolve
#This function checks for the inverse of the given matrix, if it exists then the function
#returns the value and if not, it will compute.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
                return(j)# returns the value of matrix if exists
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j#the solved value of matrix
}
#The end






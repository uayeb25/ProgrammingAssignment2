## Program Assignment 2:
## Made by Uayeb Caballero


## make a matrix with functions to set and get its values 

makeCacheMatrix <- function(x = matrix()) {
  
    s <- NULL
    x <- testMatrix(x)
    
    set <- function(y=""){
        x <<- testMatrix(y)
        s <<- NULL
    }
    get <- function() x
    
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}

## Verify if it is a square matrix

testMatrix <- function(m){
    if(!is.matrix(m)){
        message("The argument should be a square matrix")
    }else{
        if( nrow(m) == ncol(m) ){
            if( nrow(m) > 1 ){
                m
            }else{
                message("the matrix must be greater to 1 dimension")
            }
        }else{
            message("the matrix setting isn't square")
        }    
    }
    
}


## save on cache the solve of matrix

cacheSolve <- function(x, ...) {
    
    s <- x$getsolve()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    
    m <- x$get()
    if(is.null(m[1,1])){
        message("Please set correct matrix value!")
    }else{
        if(det(m)==0){
            message("The matrix's determinant should be distinct to zero")
        }else{
            s <- solve(m)
            x$setsolve(s)
            s
        }   
    }
    
}

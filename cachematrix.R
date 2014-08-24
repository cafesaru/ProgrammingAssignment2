## Put comments here that give an overall description of what your
## Course: R Programming
## Peer Assessments /Programming Assignment 2: Lexical Scoping
## By: Carlos Sánchez Rudin
## On: 2014/08/24



## makeCacheMatrix
## Creates an matrix 'object' capable capable of storing 
## the cache of its own inverse matrix
## Input: x is a numeric matrix size D x D
## Output: a self object of makeCacheMatrix
## How to create an example matrix
## Random elements up to 100 and size 5 x 5
## > mat1 <- makeCacheMatrix(matrix(sample.int(100, 5*5, TRUE), 5, 5))
makeCacheMatrix <- function(x = matrix()) {
	## m is the inversed matrix, initially NULL
	m <- NULL
	
	## set function stores the original matrix 
	## and set NULL the inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get function returns the original matrix
        get <- function() x
        
        ## setinverse function calculates and stores the 
        ## inverse matrix of the original one
        setinverse <- function(solve) m <<- solve
        
        ## setinverse function returns the stored inversed matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve
## Returns or set the invesed matrix of an 'object' makeCacheMatrix
## Input: x is an 'object' makeCacheMatrix
## Output: the inversed matrix of the original matrix in makeCacheMatrix
## How to get the inverse of the example matrix
## > cacheSolve(mat1)
cacheSolve <- function(x, ...) {
        ## Get the current inversed matrix in x
	m <- x$getinverse()
	
        if(!is.null(m)) {
        	## If there is already an inversed matrix then return it
                message("getting cached data")
                return(m)
        }
        
        ## If there is none inverse matrix calculate it and save it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

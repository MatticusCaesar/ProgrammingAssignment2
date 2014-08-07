## Matt Marchand
## last updated: 8/7/2014
## For Coursera R Programming course
## Programming assignment #2

## The basic construct of these functions is taken from Prof. Roger Peng's
## example scripts: makeVector and cachemean

## First of two function scripts to cache/calculate the inverse of a matrix
## This one creates a matrix object that can have its inverse cached.

makeCacheMatrix <- function(source_matrix = matrix()) {
	## define initial empty inverse matrix
    inverse_of_matrix <- NULL
	
	## function to define the matrix 
    set_source_matrix <- function(matrix) {
        source_matrix <<- matrix
        inverse_of_matrix <<- NULL
    }
	
	## retrieve the source matrix 
	## (just a simple return of the source_matrix variable)
    get <- function() {  ## This is an embedded function so no special name 
	    source_matrix  
	}
	
	## function to set the inverse of the source matrix
	set_matrix_inverse <- function(solve) {  ## "solve" computes the inverse of a matrix
	    inverse_of_matrix <<- solve
	}
	
	## function to get the inverse of the source matrix 
	## (simple return of the inverse_of_matrix variable)
    get_matrix_inverse <- function() 
	{
	    inverse_of_matrix
	}
	
	## return a list of the functions/outputs 
	## (all one command, just split on multiple lines for easy-reading)
	## idea taken from the makeVector example.
    list(set_source_matrix = set_source_matrix, 
	     get = get, 
		 set_matrix_inverse = set_matrix_inverse, 
		 get_matrix_inverse = get_matrix_inverse)
}


## Second of two function scripts to cache/calculate the inverse of a matrix
## This one will check for the presence of an already cached matrix.
## If it exists, it simply returns it.  If not, it calculates a 
## new matrix inverse and sets it in the cache.

cacheSolve <- function(check_matrix, ...) {

    ## Return a matrix that is the inverse of the makeCacheMatrix object matrix
    inverted_matrix <- check_matrix$get_matrix_inverse()

    ## Check whether the inverse is already set
	## If so, then just grab and return the cached matrix
    if(!is.null(inverted_matrix)) {
            return(inverted_matrix)  ## function will stop here if 
	}								 ## the cached matrix isn't empty

    ## Get the object source matrix from the makeCacheMatrix function
    matrix_to_invert <- check_matrix$get()  ## This is an embedded function so no special name

    ## Use matrix multiplication to find the inverse
    inverted_matrix <- solve(matrix_to_invert) %*% matrix_to_invert

    ## Set the inverse to the makeCacheMatrix function's cache object
    check_matrix$set_matrix_inverse(inverted_matrix)

    ## Return the inverted matrix
    inverted_matrix
}
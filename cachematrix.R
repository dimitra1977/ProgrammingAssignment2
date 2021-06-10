################################################################################
## Programming Assignment-2: Lexical Scoping
## https://www.coursera.org/learn/r-programming/home/welcome
## 
##      Author        Date        Change History
##      ~~~~~~        ~~~~        ~~~~~~~~~~~~~~
## Dipankar Mitra   06/10/2021    Initial version forked from Github.
################################################################################

## Creates 'special' matrix that can have inverse cached

makeCacheMatrix <- function(x = matrix()) 
{
    mat_inverse <- NULL
    ## Member function for setting the matrix to the matrix specified
    set_matrix <- function(m)
    {
    	## print("Setting the input matrix")
    	x <<- m
    	mat_inverse <<- NULL
    }
    
    ## Member function for getting the matrix
    get_matrix <- function()
    {
    	x
    }
    
    ## Member function for setting inverse to the passed matrix
    set_inverse <- function(y)
    {
    	## print("Setting the inverse of the matrix")
    	
    	mat_inverse <<- y
    }
    
    ## Member function for getting inverse
    get_inverse <- function()
    {
    	mat_inverse
    }
    
    ## Return the list of member functions
    list(set = set_matrix, get = get_matrix, set_inverse = set_inverse, 
    	 get_inverse = get_inverse)

}


## Calculates inverse of a cached matrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	inv <- x$get_inverse()

	if (is.null(inv))
	{
		## print("Inverse is found be be NULL!")
		mat <- x$get()
		inv <- solve(mat)
		## Set the inverse that was just calculated
		x$set_inverse(inv)
		
	}
	else 
	{
		## print("Inverse exists!")
		message("Getting cached data.")
	}	
	inv
}

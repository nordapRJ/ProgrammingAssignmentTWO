## This functions are created in order to cache the inverse of a matrix
## with the intention to save time in ponentially costly calculations

## The following is the "main function" that stores a list of functions
## that hold and replace the value of the original matrix and 
##of the inverse.

makeCacheMatrix <- function(x = matrix()) {
		inv<-NULL
		replace<-function (y){
						##changes the matrix stored in the main function
			x<<-y
			inv<<-NULL
		}
		get<-function() x       ##get´s the matrix stored in the main function
		setInv<-function(Inverse) inv<<-Inverse ## stores the value of the inverse
		getInv<-function() inv            ## gets the value of the inverse
		list(replace=replace,get=get,setInv=setInv,getInv=getInv)		
}


## This function returns the inverse of a square matrix if and only if
## the calculation wasn´t realized before. Otherwise, it retrives the 
## matrix from the cache. 

cacheSolve <- function(x, ...) {
        ## x is the "main function" created before where the matrix and
	  ## functions are stored
		inv<-x$getInv()
		if(!is.null(inv)){
			message("getting cached data")
			return(inv)
		} 
		mat<-x$get()
		inv<-solve(mat)
		x$setInv(inv)
		inv
	
}


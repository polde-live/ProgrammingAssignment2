## R Programming: Assignment 2

## The goal of this function is to create a set of objects and function to cache
## the inverse of a matrix x. The inverse of: xt should be calculated only once,
## therefore each call of function that is calculating the inverse of matrix
## will check if the inverse is already cached.


## makeCacheMatrix function is creating an object to hold the original matrix, 
## its inverse and a set of get and set methods to get the original data and 
## the inverse. 
## Argument: invertible matrix x
## Output: list of function objects to get and set the value of x and the 
## inverse x (xt)

## There is no actual inversion of the x matrix, this is done in separate
## function: cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    
    ## xt - the inverse of x matrix
    xt <- NULL;
    ## function that sets the x matrix and resets the inverse
    set <- function (y) {
        
        x <<- y;
        xt <<- NULL;
        
    }
    
    ## get function to return original matrix
    get <- function() x;
    
    ## function to set the  inverse matrix
    setinverse <- function(xt_in) xt <<- xt_in;
    
    ## function to get the inverse matrix
    getinverse <- function() xt;
    
    ## Returning list of functions to be called later
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse);
    
}


## cacheSolve function uses the matrix object, created with the makeCacheMatrix 
## function, evaluates if the object has the inverse already cached. If not the
## inverse is calculated, put to the matrix object and then returned.
## Argument: matrix object created with the makeCacheMatrix() function
## Output: inverse of matrix stored in matrix objects, passed as function 
## parameter, calculated or retrieved from cache.  

cacheSolve <- function(x, ...) {
    ## get the data from the matrix object
    xt <- x$getinverse();
    
    ## check if inverse is already cached
    ## if yes, print message and return the inverse
    if (!is.null(xt)) {
        message("getting cached inverse matrix");
        return(xt);
        
    }
    ## otherwise calculate the inverse matrix, put it to matrix object and
    ## return an inverse 
    message("calculating and caching the inverse matrix");
    
    ## get the original matrix from the object
    x_matrix <- x$get();
    
    ## calculate the inverse with solve(function)
    xt <- solve(x_matrix);
    print (xt);
    x$setinverse(xt);
    return (xt);
    
}

## Testing the functions:

## Test data and desired output 
xtest <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3);
xtest_inv <- solve(xtest);

print (xtest);
print (xtest_inv);

## Make matrix object
x_object <- makeCacheMatrix(xtest);

print (x_object$get() == xtest); #Should be resolved true
print (is.null(x_object$getinverse())); #Should be resolved true

## Calling caching function for the first time; 
## should print "calculating and caching the inverse matrix"
x_inversed <- cacheSolve(x_object);


print (is.null(x_object$getinverse())); #Should be resolved false


print (x_object$getinverse() == xtest_inv);#Should be resolved true

## Calling the cashing function for a second time; 
## should print "getting cached inverse matrix"
x_inversed <- cacheSolve(x_object);

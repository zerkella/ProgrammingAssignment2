# Special matrix functions to work with matrices and their cached inversions 

# Create an "object" which contains a matrix "m" and its inverse "inverse"
# Expose the API: set(), get(), setInverse, getInverse()
makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL;

    set <- function (value) {
        m <<- value;
        inverse <<- NULL;
    };
    
    get <- function () {
        m;
    }
    
    setInverse <- function (value) {
        inverse <<- value;
    };
    
    getInverse <- function () inverse;
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse);
}

# Calculate the inverse of CacheMatrix "m", but try to used cached inverse value, if any.
cacheSolve <- function(m, ...) {
    result <- m$getInverse();
    if (is.null(result)) {
        message("calculating inverse");
        result <- solve(m$get());
        m$setInverse(result);
    }
    result;
}

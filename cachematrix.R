## makeCacheMatrix：创建特殊矩阵对象，缓存它的逆矩阵
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # set：重置矩阵，清空缓存的逆
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get：获取原始矩阵
  get <- function() x
  # setinv：保存逆矩阵到缓存
  setinv <- function(inverse) inv <<- inverse
  # getinv：读取缓存的逆矩阵
  getinv <- function() inv
  
  # 返回列表，暴露4个内部函数
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve：计算/读取缓存的矩阵逆
cacheSolve <- function(x, ...) {
  ## 先读取缓存里的逆
  inv <- x$getinv()
  # 如果缓存不为空，直接返回缓存值
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # 缓存为空，计算逆矩阵
  data <- x$get()
  inv <- solve(data, ...)
  # 存入缓存
  x$setinv(inv)
  inv
}

#inspiration came from: https://www.w3resource.com/c-programming-exercises/array/c-array-exercise-28.php
determinant = function(mrx){
  mtrx = matrix(data = mrx, nrow = 3, ncol = 3)
  det = 0
  for(i in 1:3){
    twoOver = ((i + 1) %%3) 
    threeOver = ((i + 2) %%3) 
    if(twoOver ==0) twoOver = 3 
    if(threeOver == 0) threeOver = 3
    det = det + (mtrx[1,i]*(mtrx[2,twoOver] * 
      mtrx[3,threeOver] - mtrx[2,threeOver] * mtrx[3,twoOver]));
  }
  return(det)
}
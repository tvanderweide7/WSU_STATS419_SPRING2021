# http://sanfoundry.com/c-program-compute-determinant-matrix/
determinant = function(mrx){
  mtrx = matrix(data=mrx, nrow = 3, ncol = 3)
  det =    mtrx[1,1] * ((mtrx[2,2]*mtrx[3,3]) - (mtrx[3,2]*mtrx[2,3])) -mtrx[1,2] * (mtrx[2,1]
    * mtrx[3,3] - mtrx[3,1] * mtrx[2,3]) + mtrx[1,3] * (mtrx[2,1] * mtrx[3,2] - mtrx[3,1] * mtrx[2,2]);
  return(det)
}
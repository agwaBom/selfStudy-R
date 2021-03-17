### SVD calculation
### A = UZ(Σ)V^t
### W*eigenVector(x) = eigenValue(ㅅ)*eigenVector(x)
### (W-eigenValue(ㅅ))*eigenVector(x) = 0
A = matrix(c(3,2,0,0,6,3,0,0), nc = 2, byrow = FALSE)
t(A) # transpose matrix

## multiplies two matrices if conformable
# A*At
W = A%*%t(A)

## calculate EigenValue & Vector of W
# EigenValue
eigen(W)[[1]]
# EigenVector
eigen(W)[[2]]

## U is EigenVector of W
U = eigen(W)[[2]]

## Calculation of V^t
W1 = t(A)%*%A

# V^t is eigenVector of W1
V = eigen(W1)[[2]]

## calculation of Σ(Z)
W1_eigenValue_sqrt = sqrt(eigen(W1)[[1]]) # square root of eigenVector of W1
W1_eigenValue_sqrt

# initialize matrix with 0
Z = matrix(rep(0,8), nc=2, byrow=F)
Z

# insert square root of eigenValue W1 at each (1,1), (2,2)
Z[1,1] = W1_eigenValue_sqrt[1]
Z[2,2] = W1_eigenValue_sqrt[2]


A # original
U # eigenVector of A*t(A)
Z # square root of eigenValue of t(A)*A
V # eigenvector of t(A)*A
SVD_of_A = U%*%Z%*%t(V)

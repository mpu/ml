/ notes -
/ adding : after an operator takes the unary version

dp:+/*            / dot product
mm:{x dp/:\: +y}  / matmult

/ x is (2,n)  y is (1,n)  w is (1,2)
/ l: loss function; sum of squares; (1,n) to 1
/ f: {l[mm[w;x] - y]}
/ D_xyf = D_(mm[w;x]-y])l o D_(xy)(mm[w;x] - y)
/         (2x1 2x2 ... 2xn)
/         2(w1x11+w2x21-y1 w1x21+w2x22-y2 ...)
/                           (x11 x21; x12 x22; ...)

fwd:{[w;x] mm[,w;x]0} / forward function

W:0.5 1.5
x:2 10#20?10   / training data
y:fwd[W;x]     / training labels

gd:{[w] mm[,2*(fwd[w;x]-y);+x]0}  / loss gradient
step:{[w] w-0.001*gd[w]}          / descent step
30 step\ 2?5                      / train

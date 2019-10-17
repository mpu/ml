/ notes -
/ adding : after an operator takes the unary version

dp:+/*            / dot product
mm:{x dp/:\: +y}  / matmult
sig:%1+exp-:      / sigmoid

/ y1 = x1*w11 + x2*w12   y2 = x1*w21 + x2*w22
/ dy1/dw = (x1 x2 0  0 )
/ dy2/dw = (0  0  x1 x2)
/ dy1/dx = (w11 w12)
/ dy2/dx = (w21 w22)

/ x is (2,n)  y is (1,n)  w is (1,2)
/ l: loss function; sum of squares; (1,n) to 1
/ f: {l[mm[w;x] - y]}
/ D_xyf = D_(mm[w;x]-y])l o D_(xy)(mm[w;x] - y)
/         (2x1 2x2 ... 2xn)
/         2(w1x11+w2x21-y1 w1x21+w2x22-y2 ...)
/                           (x11 x21; x12 x22; ...)

/ Df = [df/dw1 df/dw2]


fwd:{[w;x] mm[,w;x]0} / forward function

W:0.5 1.5
x:2 10#20?10   / training data
y:fwd[W;x]     / training labels

gd:{[w] mm[,2*(fwd[w;x]-y);+x]0}  / gradient
step:{[w] w-0.001*gd[w]}          / descent step
30 step\ 2?5

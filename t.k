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

/ k-means (k=2 on 1d data)
ds:("ff";" ")0:"data/2means.txt"  / load data
xs:ds 0                           / points

/ an iteration of k-means
/ d: distance to each centroid
/ a: indices of data belonging to each centroid
km:{d:abs x-\:xs;a:&:'d=\:&/d;avg'xs'a}
km\xs[2?#xs]    / iterate starting from 2 samples

/ n-grams
ng:{[n;l] (n-1)_(n#0){1_x,y}\l}

cut:{1_'(&x=*x)_x:" ",x}    / cut words
lc:{`c$x+32*(x in 65+!26)}  / lower case
kp:{x^".,-_!?"}             / kill punctuation

hg:0:"data/hegel.txt"       / read the text
hg:(kp'lc',/cut'hg)^,""     / normalize it

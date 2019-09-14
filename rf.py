import numpy as np
import matplotlib.pyplot as pl

# One decision tree
class DT:
    dim = 0 # dimension to compare to thr
    thr = 0 # threshold
    odds = (0., 0.) # binary classification
    suby = None     # yes: data > thr
    subn = None     # no:  data <= thr

    def setodd(self, p):
        self.odds = (p, 1-p)
    
    def classify(self, data):
        if self.suby is None or self.subn is None:
            return self.odds
        if data[self.dim] > self.thr:
            return self.suby.classify(data)
        else:
            return self.subn.classify(data)

def entropy(y):
    # y is a vector of 0 or 1
    N = y.size
    p = np.sum(y)
    n = N - p
    if p == 0 and n == 0:
        return 0
    if p == 0:
        return - (n/N)*np.log(n/N)
    if n == 0:
        return - (p/N)*np.log(p/N)
    return \
        - (p/N)*np.log(p/N) \
        - (n/N)*np.log(n/N)

def choice(xs, N):
    sel = np.random.choice(xs.shape[0], N)
    return xs[sel, :]

DBG = False

# learn a random decision tree
def learn(lvl, xs, y):
    assert len(xs.shape) == 2
    assert xs.shape[1] == 2
    assert len(y.shape) == 1
    dt = DT()
    N = y.size
    dt.setodd(np.sum(y) / N)
    if lvl == 0:
        # max depth reached
        return dt
    splits = choice(xs, max(2, int(np.sqrt(N))))
    bestred = -1
    bestyes = None
    bestno = None
    bestdim = None
    bestthr = None
    for dim in range(2):
        for s in splits:
            yes = xs[:, dim] > s[dim]
            no = np.logical_not(yes)
            ny = np.sum(yes)
            nn = N - ny
            red = entropy(y) \
                - (ny/N)*entropy(y[yes]) \
                - (nn/N)*entropy(y[no])
            if DBG:
                print("N:{} ny:{}={}+{} nn:{}={}+{} cut:{},{} red: {}".format(
                    N, ny,
                    ny - np.sum(y[yes]),np.sum(y[yes]),
                    nn,
                    nn - np.sum(y[no]),np.sum(y[no]),
                    dim, s[dim],
                    red))
            if red > bestred:
                bestred = red
                bestyes = yes
                bestno = no
                bestdim = dim
                bestthr = s[dim]
    if DBG:
        print("At level {}, got entropy reduction of: {}\n".format(
            lvl, bestred))
    if bestred != 0:
        assert not (bestyes is None or bestno is None)
        dt.dim = bestdim
        dt.thr = bestthr
        dt.suby = learn(lvl-1, xs[bestyes], y[bestyes])
        dt.subn = learn(lvl-1, xs[bestno], y[bestno])
    return dt

def classify(ts, data):
    p0 = 0
    p1 = 0
    for t in ts:
        (q0, q1) = t.classify(data)
        p0 += q0
        p1 += q1
    p0 /= len(ts)
    p1 /= len(ts)
    return (p0, p1)

# -- Test
noise = lambda: np.random.normal(scale=0.04, size=(50)) 
noise = lambda: 0

t = np.linspace(-1, 1)
xr = -.1 + np.cos(3.14 * t) * (t+1)/2 + noise()
yr = np.sin(3.14 * t) * (t+1)/2 + noise()
xg = .1 + np.cos(3.14 * (t+1)) * (t+1)/2 + noise()
yg = np.sin(3.14 * (t+1)) * (t+1)/2 + noise()

X = np.array([
        np.concatenate((xr,xg)),
        np.concatenate((yr,yg)) ]).T
Y = np.concatenate((
        np.zeros_like(xr),
        np.ones_like(xg)))

TD = 5 # tree deptb
TC = 20 # tree count

ts = []
for _ in range(TC):
    # TOOD: bagging
    ts.append(learn(TD, X, Y))

print("-- Showtime...")

numok = 0
for i in range(Y.size):
    (p0, p1) = classify(ts, X[i])
    print("classification: (1){:.3f} (0){:.3f} label: {}".format(p0, p1, Y[i]))
    if p0 > p1:
        if Y[i] == 1:
            numok += 1
    else:
        if Y[i] == 0:
            numok += 1

print("Precision: {:.3f}".format(numok / Y.size))

res = 200
c = np.ndarray((res, res))
for i in range(res):
    for j in range(res):
        x = 1.4 * (i - res/2) / (res/2)
        y = 1.4 * (res/2 - j) / (res/2)
        c[i][j] = classify(ts, [x,y])[0]

pl.imshow(c.T, extent=(-1.4,1.4,-1.4,1.4))
pl.plot(xr, yr, 'ro')
pl.plot(xg, yg, 'go')
pl.show()

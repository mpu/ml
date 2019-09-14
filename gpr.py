# gaussian process regression
# Theory: [y, y*] ~ N([mu, mu*], [K, K*][K*T, K**])
#  - y(x) is the data given
#  - y* is to be inferred
# Use theorem on Gaussians (convolution) to get
# conditional for y*: P(y* | y, x, mu, mu*, K*, K**)
# This is a Gaussian again. K* and K** are taken
# from a so-called kernel to be designed; a typical
# choice is exponential squared (see in code)
import numpy as np
import matplotlib.pyplot as pl

def sqdist(a, b):
    a = a[np.newaxis].T
    return a**2 + b**2 - 2*a*b

def kernel(a, b):
    return np.exp(-0.5 * sqdist(a, b))

n = 50
Xtest = np.linspace(-5, 5, n)
Xdata = Xtest[np.abs(Xtest) < 1]
#Ydata = 2*np.sin(2*Xdata)  # function to approximate
Ydata = Xdata ** 2 - 0.5

# kernel for the given data
K_ = kernel(Xdata, Xdata)
# Add 1e-6, to account for the noise (variance) of the input
# data
Ky = K_ + 1e-6*np.eye(Xdata.size)

# Using L is supposedly more stable than inverting K straight
# away; on this example, it does not seem to change anything
L = np.linalg.cholesky(Ky)
#alpha = np.linalg.solve(L.T, np.linalg.solve(L, Ydata))

alpha = np.linalg.solve(Ky, Ydata)

kstars = kernel(Xtest, Xdata)
mean = np.dot(kstars, alpha)
print("kstars: {}  alpha: {}  mean: {}".format(kstars.shape, alpha.shape, mean.shape))

pl.plot(Xtest, mean)
pl.plot(Xdata, Ydata, 'ro')
pl.show()

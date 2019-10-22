# generate some stuff in data/
import numpy as np
import matplotlib.pyplot as pl

d0=np.random.normal(loc=0,scale=0.3,size=60)
d1=np.random.normal(loc=1.0,scale=0.3,size=60)

z0=np.stack((d0,np.zeros_like(d0))).T
z1=np.stack((d1,np.ones_like(d1))).T
ds=np.concatenate((z0,z1))

print(ds.shape)
np.savetxt('data/2means.txt',ds,fmt='%f')  # damn format is important for k interop

pl.plot(d0,np.zeros_like(d0),'d')
pl.plot(d1,np.zeros_like(d1),'d')
pl.show()

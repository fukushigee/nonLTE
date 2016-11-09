from pylab import *
import numpy as np
from matplotlib import pyplot as plt

head = ("head","<i")
tail = ("tail","<i")


N=128 #grid
lev=2 #level
dt = np.dtype([head,("TBIvel","<81920f"),tail])     #81920=128*128*5,,,32768=128*128*2

fd = open("spnnnlev.b","rb")  #spnlev.b
data = np.fromfile(fd, dtype=dt, count=-1) #bynal


intensity = data[0]["TBIvel"].reshape((128,128,-1),order="F")
print intensity


plt.xlabel('x')
plt.ylabel('y')


plt.xlim(0,N-1)
plt.ylim(0,N-1)



x = arange(N)
y = arange(N)
Z = array(intensity[0:127,0:127,lev])


X, Y = meshgrid(x, y)

pcolor(X, Y, Z)
#plt.contour(X,Y,Z)
plt.colorbar()
show()


from pylab import *
import numpy as np
from matplotlib import pyplot as plt

def readbi(failname,grid,nlev):

	head = ("head","<i")
	tail = ("tail","<i")
	
	yoso = "<"+str(grid*grid*nlev)+"f"

	f=open(filename,"rb")  #spnlev.b
	dt = np.dtype([head,("TBIvel",yoso),tail])
	data = np.fromfile(f, dtype=dt, count=-1) #bynal
	matrix = data[0]["TBIvel"].reshape((grid,grid,-1),order="F")
	return matrix


GRID=128  #input
NLEV=5    #input
level=2   #input
filename = "spnnnlev.b"
intensity = readbi(filename,GRID,NLEV)
print intensity


plt.xlabel('x')
plt.ylabel('y')


plt.xlim(0,GRID-1)
plt.ylim(0,GRID-1)

x = arange(GRID)
y = arange(GRID)
Z = array(intensity[0:GRID-1,0:GRID-1,level])


X, Y = meshgrid(x, y)

pcolor(X, Y, Z)
#plt.contour(X,Y,Z)
plt.colorbar()
show()

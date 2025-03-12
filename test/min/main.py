from munkres import Munkres, print_matrix
import numpy as np

mo = Munkres()

m = np.loadtxt("m",dtype='i')
D = len(m)
#print(m)
#print(D)

m2 = np.zeros((D,D))
#print(m2)

for i in range(0,D):
    for j in range(0,D):
        m2[i][j] = m[i][j]

indexes = mo.compute(m2)
#print_matrix(m, msg='Lowest cost through this matrix:')
total = 0
for row, column in indexes:
    value = m[row][column]
    total += value
#    print(f'({row}, {column}) -> {value}')
#print(f'total cost: {total}')

for row, col in indexes:
    print("%02d" % (col+1),end='')
print(f':{total}')

"""Volume 2: Simplex

Neil Thompson
1 March 2022
323.2
"""

import numpy as np


# Problems 1-6
class SimplexSolver(object):
    """Class for solving the standard linear optimization problem

                        minimize        c^Tx
                        subject to      Ax <= b
                                         x >= 0
    via the Simplex algorithm.
    """
    # Problem 1
    def __init__(self, c, A, b):
        """Check for feasibility and initialize the dictionary.

        Parameters:
            c ((n,) ndarray): The coefficients of the objective function.
            A ((m,n) ndarray): The constraint coefficients matrix.
            b ((m,) ndarray): The constraint vector.

        Raises:
            ValueError: if the given system is infeasible at the origin.
        """
        # catch the unfeasible system
        if not np.alltrue(b>=0):
            raise ValueError("The given system is infeasible at the origin")
        
        # call the generation
        self._generatedictionary(c, A, b)
        

    # Problem 2
    def _generatedictionary(self, c, A, b):
        """Generate the initial dictionary.

        Parameters:
            c ((n,) ndarray): The coefficients of the objective function.
            A ((m,n) ndarray): The constraint coefficients matrix.
            b ((m,) ndarray): The constraint vector.
        """
        # save attributes
        m = len(b)
        n = len(c)
        self.m = m
        self.n = n
        
        # build dictionary
        cbar = np.hstack((c, np.zeros_like(b)))
        Abar = np.hstack((A, np.eye(m)))
        self.D = np.vstack((np.hstack((0, cbar)),np.hstack((b.reshape(m,1), -Abar))))


    # Problem 3a
    def _pivot_col(self):
        """Return the column index of the next pivot column.
        """
        # return the first negative value of the top line (besides the first entry)
        return np.argmax(self.D[0, 1:]<0)+1
        

    # Problem 3b
    def _pivot_row(self, index):
        """Determine the row index of the next pivot row using the ratio test
        (Bland's Rule).
        """
        # check for unboundedness
        if np.alltrue(self.D[1:, index]>=0):
            raise ValueError("All entries in pivot column are non-negative - unbounded with no solution")
        
        # Bland's Rule and error checking, along with finding minimum
        i_min = 0
        r_min = np.inf
        for i in range(1, self.m+1):
            if self.D[i, index] >= 0:
                continue
            r = -self.D[i, 0]/self.D[i, index]
            if r < r_min:
                r_min = r
                i_min = i
        return i_min
            

    # Problem 4
    def pivot(self):
        """Select the column and row to pivot on. Reduce the column to a
        negative elementary vector.
        """
        col = self._pivot_col()
        r = self._pivot_row(col)
        # divide the pivot row by the negative value of the pivot entry
        self.D[r] /= -self.D[r, col]
        
        
        # use the pivot row to zero out all entries in the pivot column above and below the pivot entry
        for ri in range(self.m+1):
            if ri==r:
                continue
            d = -self.D[ri, col]/self.D[r,col]
            self.D[ri] += d*self.D[r]

        
    # Problem 5
    def solve(self):
        """Solve the linear optimization problem.

        Returns:
            (float) The minimum value of the objective function.
            (dict): The basic variables and their values.
            (dict): The nonbasic variables and their values.
        """
        # keep pivoting when there is a negative coefficient on the top row
        while not np.alltrue(self.D[0, 1:]>=0):
            self.pivot()
        
        min_obj = self.D[0,0]
        basic = dict()
        nonbasic = dict()
        
        # add results to dictionaries
        for i in range(self.n+self.m):
            i1 = i + 1
            e = self.D[0, i1]
            if e==0:
                r = np.argmin(self.D[:, i1])
                basic[i] = self.D[r, 0]
            elif e>0: 
                nonbasic[i] = 0
            else:
                raise KeyError("Not actually a KeyError, the algorithm is faulty and has negative coefficients")

        return min_obj, basic, nonbasic
    
# Problem 6
def prob6(filename='productMix.npz'):
    """Solve the product mix problem for the data in 'productMix.npz'.

    Parameters:
        filename (str): the path to the data file.

    Returns:
        ((n,) ndarray): the number of units that should be produced for each product.
    """
    # load data
    pmp = np.load('productMix.npz')
    
    # set up linear problem in standard form
    c = -pmp['p']
    n = len(c)
    A = np.vstack((pmp['A'], np.eye(n)))
    b = np.hstack((pmp['m'], pmp['d']))
    
    # solve the problem
    opt,basic,nbasic = SimplexSolver(c, A, b).solve()
    
    # return the first 4 independent variables
    units = list()
    for i in range(n):
        units.append(basic[i])
    return np.array(units)
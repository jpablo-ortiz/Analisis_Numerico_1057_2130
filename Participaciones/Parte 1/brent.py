# https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.root_scalar.html#scipy.optimize.root_scalar
import matplotlib.pyplot as plt
import numpy as np
from scipy import optimize


def function(x):
    return np.sin(x)

# a method that calculate brents root with sklearn
def brentq_root_V1(f, a, b, args=(), xtol=1e-10, rtol=1e-10, maxiter=100, full_output=False):
    return optimize.brentq(f, a, b, args=args, xtol=xtol, rtol=rtol, maxiter=maxiter, full_output=full_output)


def brentq_root_V2(f):
    sol = optimize.root_scalar(f, method='brentq', bracket=[0, 1])
    sol.root

#rentq_root_V2(function)

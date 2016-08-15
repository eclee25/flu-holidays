### packages/modules ###
import csv
import sys
import datetime as date
#import networkx as nx
import numpy as np

### local modules ###


# functions #
###################################################
def calc_R_matrix (beta, gamma, alpha):
# calculate R matrix from beta value
    
    # present contact matrix C
    C = np.matrix([[18.6, 4.2], [5.6, 8.0]])
    print 'contact matrix', C
    
    # assign components of C matrix
    C_cc = C.item((0, 0))
    C_ca = C.item((0, 1))
    C_ac = C.item((1, 0))
    C_aa = C.item((1, 1))
    
    # multiply components of matrix C by alpha or (1-alpha)
    mx_11 = (C_cc * alpha)
    #mx_11 = 7
    mx_12 = (C_ca * alpha)
    #mx_12 = 7
    mx_21 = (C_ac * (1 - alpha))
    #mx_21 = 7
    mx_22 = (C_aa * (1 - alpha))
    #mx_22 = 7
    
    # calc R matrix
    # R = np.array((beta / gamma) * (np.matrix([[mx_11, mx_12], [mx_21, mx_22]])))
    R = np.array((beta / gamma) * C)
    
    print 'alt C mx', np.matrix([[mx_11, mx_12], [mx_21, mx_22]])
    print 'next gen mx', R

    return C, R
    
###################################################    
def calc_R0 (R):
#determine R0 from largest eigenvalue of R matrix

    # calc eigenvalues from R matrix
    # should return in descending order (largest will be first)
    evals, matrix_v = np.linalg.eig(R) # matrix_v --> normalized eigenvectors
    
    print 'evals', evals
    print 'matrix_v', matrix_v

    # largest eigenvalue is R0
    R0 = max(evals)

    print 'R0', R0
       
    return R0   
    
###################################################
if __name__ == "__main__":
    
    # DEFINE DISEASE PARAMETERS
    # if beta/beta+gamma
    # if beta/gamma
    beta = 0.029
    # was beta = 0.037 with old contact matrix
    gamma = 0.5 #changed 090915 based on note on 080315
    
    # CALCULATE ALPHA
    alpha = 0.24

    # CALCULATE R MATRIX
    C, R = calc_R_matrix (beta, gamma, alpha)
    
    # CALCULATE R0
    R0 = calc_R0(R)
    

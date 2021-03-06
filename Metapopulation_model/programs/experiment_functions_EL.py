# Author: Anne Ewing
## Date: 11/16/15
## Function: script with experimental parameters and functions

import networkx as nx # to work with the population contact network
import random as rnd
import numpy as np
import matplotlib.pyplot as plt
import csv
import sys
import operator

### local modules ###
sys.path.append('/home/anne/Dropbox/Anne_Bansal_lab')

### functions ###
import population_parameters as pop_func

#######################################
def list_exp_names ():
    
    time_intervals = ['cum_incd', 'real_time']
    exp_names = ['red_C_cc', 'red_all_child', 'red_beta']
    
    return time_intervals, exp_names

#######################################
def set_time_start_and_end (model_peak, data_peak, data_holiday_start, dis_len_before, dis_len_after, trav_len_before, trav_len_after):
    # length_before = how many weeks or time_steps before xmas
    # length_after = how many weeks or time_steps after xmas
    #wk 50 on cum_incid_graph 20%
    #wk 1 - 30%
    #import CSV file with wk# and cum incidence for each season
    ## take average of cumulative incidence across seasons
    #use time_step for xmas
    ## if epi trajectory is reasonable, just use time_step = 1 day
    ### can use actual day of xmas - take average across seasons
    
    # calc cum incid percent
    #cum_ili = np.cumsum(ili_list) # or use tot infc time series
    #sum_ili = sum(ili_list) # total numb infc at end
    #d_cum_percent = ((cum_ili)/(sum_ili)) * 100
    
    #if timing == 'cum_incd': #measure in weeks
    #    xmas = 51
    #    wk_start = xmas - length_before # (51 - 2 = 49)
    #    cum_incd = d_cum_incd[(wk_start)] # 49
    #    time_step = [time for time in d_cum_incd_model[(time)] if d_cum_incd_model[(time)] == cum_incd]
    #    #time_start = #time_step when cum incidence = 20%
    #    #time_end = 
    
    epi_start = (model_peak - data_peak)
    model_holiday = (epi_start + data_holiday_start)
    dis_start = (model_holiday - dis_len_before)
    dis_end = (model_holiday + dis_len_after)
    travel_start = (model_holiday - trav_len_before)
    travel_end = (model_holiday + trav_len_after)
    
    return dis_start, dis_end, travel_start, travel_end
    
#######################################
#def set_time_end (time_units, ):
#
#    
#######################################
def reduce_C_cc (C):
    # reduce child to child contacts only
    
    C_cc = C.item((0, 0))
    C_ca = C.item((0, 1))
    C_ac = C.item((1, 0))
    C_aa = C.item((1, 1))
    red_percent = ((27.7 - 11.6) / 27.7) # 58% reduction
    #red_percent = .4
    red_C_cc = (C_cc - (C_cc * red_percent))
    C_exp = np.matrix([[red_C_cc, C_ca], [C_ac, C_aa]])
    #C.item((0,0)) = red_C_cc #reassign C_cc in contact matrix
    
    return C_exp
    
#######################################
def reduce_C_aa (C):
    
    C_cc = C.item((0, 0))
    C_ca = C.item((0, 1))
    C_ac = C.item((1, 0))
    C_aa = C.item((1, 1))
    red_percent = ((27.7 - 11.6) / 27.7) # 58% reduction
    #red_percent = .4
    red_C_aa = (C_aa - (C_aa * red_percent))
    C_exp = np.matrix([[C_cc, C_ca], [C_ac, red_C_aa]])
    #C.item((0,0)) = red_C_cc #reassign C_cc in contact matrix
    
    return C_exp

#######################################
def reduce_C_all (C):
    
    C_cc = C.item((0, 0))
    C_ca = C.item((0, 1))
    C_ac = C.item((1, 0))
    C_aa = C.item((1, 1))
    C_cc_red_percent = ((27.7 - 11.6) / 27.7) # 58% reduction
    C_ca_red_percent = ((3.8 - 2.3) / 3.8) # child contacts reported by adults
    C_ac_red_percent = ((11.2 - 11.7) / 11.2) # adult contacts reported by children
    C_aa_red_percent = ((14.8 - 15) / 14.8)
    red_C_cc = (C_cc - (C_cc * C_cc_red_percent))
    red_C_ca = (C_ca - (C_ca * C_ca_red_percent))
    red_C_ac = (C_ac - (C_ac * C_ac_red_percent))
    red_C_aa = (C_aa - (C_aa * C_aa_red_percent))
    C_exp = np.matrix([[red_C_cc, red_C_ca], [red_C_ac, red_C_aa]])
    
    return C_exp

#######################################
def reduce_C_ageROnly (C):
    # 8/11/16
    C_cc = C.item((0, 0))
    C_ca = C.item((0, 1))
    C_ac = C.item((1, 0))
    C_aa = C.item((1, 1))
    C_c_percent = ((7.78+5.83) / (C_cc+C_ac)) # 0.56 of total child contacts in orig C (scaled by C_red_all child contact colsum)
    C_a_percent = ((2.55+8.15) / (C_ca+C_aa)) # 0.87 of total adult contacts in orig C (scaled by C_red_all adult contact colsum)
    red_C_cc = (C_cc * C_c_percent)
    red_C_ca = (C_ca * C_a_percent)
    red_C_ac = (C_ac * C_c_percent)
    red_C_aa = (C_aa * C_a_percent)
    C_exp = np.matrix([[red_C_cc, red_C_ca], [red_C_ac, red_C_aa]])
    
    return C_exp

#######################################
def reduce_C_ageROnly_less (C):
    # 9/1/16
    C1 = reduce_C_ageROnly(C)
    C_exp = 0.9*C1 # 0.9 of partial school closure intervention
    
    return C_exp    

#######################################
def reduce_C_ageROnly_more (C):
    # 9/1/16
    C1 = reduce_C_ageROnly(C)
    C_exp = 1.1*C1 # 1.1 of partial school closure intervention
    
    return C_exp    

#######################################
# main 

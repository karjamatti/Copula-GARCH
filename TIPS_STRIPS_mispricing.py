'''
Created on Mar 16, 2019

author: Matti Karjalainen
Github: karjalm5
email: karjamatti@gmail.com

This script was used for the course project on the spring 2019 edition of the "Fixed Income" course
at the Aalto University School of Business, Dept of Finance.

The input files contain data from Thomson Reuters DataStream.

This script calculates the theoretical relative mispricing of Inflation-linked STRIPS (TIPS STRIPS).

Input data:
1. Inflation swap rates
2. Inflation-linked STRIP prices
3. Coupon or principal STRIP prices
'''
from scipy.interpolate import CubicSpline
import numpy as np
import pandas as pd
from datetime import datetime

'''
STRING PARAMETERS
'''
sep = ","
output_file = 'STRIPS_mispricing.txt'
dateformat = '%m/%d/%Y'
dateformat2 = '%d/%m/%Y'
enc = "utf-8"
titles = ['Date', 'Average_Relative_Mispricing', 'Average_Maturity', 'Number_of_STRIP-pairs']

'''
INPUT FILES
'''
# INFLATION SWAPS
# This file should consist of the daily inflation swap rates for different maturities
# First column should contain date (m/d/y), and other columns should have...
# ...inflation swap rates with maturities 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 25, 30
swaps = pd.read_csv('swaps.csv', sep = sep)

# INFLATION LINKED STRIPS
# This file should contain the prices of coupons stripped from inflation-linked bonds
# First column should contain date (m/d/y), and other columns should have...
# ...coupon prices for different maturity dates, where the maturity is the column header (d/m/y)
infstrips = pd.read_csv('infstrips.csv', sep = sep)
infstrips = infstrips.loc[:, ~infstrips.columns.str.replace("(\.\d+)$", "").duplicated()] # This is to get rid of duplicate columns in the data
infstrips = infstrips.dropna(how='all', axis=1)

# COUPON STRIPS
# This file should contain the prices of coupons stripped from regular bonds
# First column should contain date (m/d/y), and other columns should have...
# ...coupon prices for different maturity dates, where the maturity is the column header (d/m/y)
cstrips = pd.read_csv('cstrips.csv', sep = sep)
cstrips = cstrips.loc[:, ~cstrips.columns.str.replace("(\.\d+)$", "").duplicated()] # This is to get rid of duplicate columns in the data
cstrips = cstrips.dropna(how='all', axis=1)

# PRINCIPAL STRIPS
pstrips = pd.read_csv('pstrips.csv', sep = sep)
pstrips = pstrips.loc[:, ~pstrips.columns.str.replace("(\.\d+)$", "").duplicated()] # This is to get rid of duplicate columns in the data
pstrips = pstrips.dropna(how='all', axis=1)

# CHOOSE STRIP TYPE
# This is to decide whther to use coupon or principal strips. Should not make that big of a difference really..
strips = cstrips

'''
DATE & MATURITY LISTS
'''
dates = infstrips.loc[:, 'Unnamed: 0'] # Get the series of observation dates from the infstrips-file
is_mat = infstrips.columns.values.tolist()[1:] # Get the list of maturities (dates) for the inflation-linked strips
st_mat = strips.columns.values.tolist()[1:] # Get the list of maturities (dates) for the regular strips
sw_mat = list(swaps.columns.values)[1:] # Get the list of maturities (years) for the inflation swaps

'''
LOOP COUNTERS FOR DATAFRAMES
'''
cols = dates.shape[0]
is_rows = len(is_mat)
st_rows = len(st_mat)
sw_rows = len(sw_mat)

'''
HELPER FUNCTIONS
'''
def find_spline(x, y): # This function returns the spline for interpolating mismatched maturities
    x = [float(i) for i in x]
    y = [float(i) for i in y]
    spline = CubicSpline(x, y)
    return spline

def interpolate_vals(values, spline): # This function interpolates the values for the mismatched maturities
    intvals = spline(values)
    return intvals

def cubic_spline(spread, matches): # This brings the two functions above together
    spline = find_spline(sw_mat, spread)
    interpolated_rates = interpolate_vals(matches, spline)
    return interpolated_rates

def dict_average(dict_var): # Finds the average of values in a dictionary
    sumvar = 0
    for key, value in dict_var.items():
        sumvar += value
    if len(dict_var) != 0:
        average_value = sumvar / len(dict_var)
    else:
        average_value = 'NA'
    return average_value

def average_maturity(swapyears): # Finds the average maturity of all trading opportunities at a given time
    if len(swapyears) != 0:
        avg_mat = np.mean(np.asarray(swapyears))
    else:
        avg_mat = 0
    return avg_mat

def write_results(vec): # Writes the time series of average mispricing and maturities to a text file
    output = open(output_file, "w", encoding = enc)
    for title in titles:
        output.write(title + " ")
    output.write("\n")
    for key, value in vec.items():
        output.write(str(key) + " " + str(value) + "\n")
    output.close()

'''
MAIN FUNCTION
'''
def main():

    mastervec = [] # INITIALIZE TIME-SERIES VECTOR OF RESULTS

    # LOOP THROUGH DAILY DATA OF SECURITIES TO CALCULATE RELATIVE MISPRICINGS
    for i in range(cols):

        date = datetime.strptime(dates[i], dateformat) # CONVERT DATE FROM STR TO DATETIME-OBJECT
        matchdate = datetime(2019,3,15)

        if date == matchdate:

            infstrip = infstrips.iloc[i,:].tolist()[1:] # INFLATION LINKED STRIP PRICES: CONVERT DATAFRAME ROW TO LIST
            strip = strips.iloc[i,:].tolist()[1:] # STRIP PRICES: CONVERT DATAFRAME ROW TO LIST
            swap = swaps.iloc[i,:].tolist()[1:] # INFLATION SWAP SPREADS: CONVERT DATAFRAME ROW TO LIST

            is_dict, st_dict = {}, {} # INITIALIZE DICTIONARIES FOR AVAILABLE MATURITY-STRIP -PRICE PAIRS

            # INFLATION LINKED STRIPS
            for j in range(is_rows):
                if not np.isnan(infstrip[j]): # DISCARD 'nan's
                    is_dict[is_mat[j]] = infstrip[j] # ADD PRICE TO DICTIONARY UNDER MATURITY-KEY

            # STRIPS
            for k in range(st_rows):
                if not np.isnan(strip[k]):
                    st_dict[st_mat[k]] = strip[k]

            matches = [] # INITIALIZE LIST FOR MATURITY MATCHES

            # CHECK FOR INFL-STRIP MATURITIES IN STRIP-DICTIONARY TO FIND PAIRS
            for key in is_dict:
                if key in st_dict:
                    matches.append(key)

            # CONVERT MATURITIES TO YEARS FROM DATE
            swapyears = [((datetime.strptime(i, dateformat2) - date).days)/365 for i in matches]

            # INTERPOLATE THEORETICAL SWAP RATES FOR MATCHED MATURITIES
            swaprates = cubic_spline(swap, swapyears)

            # INITIALIZE DICTIONARY FOR PRICING MISMATCHES
            mismatch = {}

            # LOOP THROUGH MATCHES
            for i in range(len(matches)):

                facevalue = 100

                reg_strip = st_dict[matches[i]] # STRIP PRICE
                i_strip = is_dict[matches[i]] # INFLATION LINKED STRIP -PRICE
                s_rate = swaprates[i] # INFLATION SWAP -RATE
                s_year = swapyears[i] # TIME TO MATURITY IN YEARS
                is_per_fv = (1+s_rate/facevalue)**(s_year) * facevalue # CASH FLOW FROM INFLATION LINKED STRIPS AFTER INFLATION SWAP
                is_required = facevalue/is_per_fv # REQUIRED FACE VALUE OF INFLATION STRIPS FOR A NOMINAL VALUE OF 100 AFTER SWAP
                price = is_required * i_strip # TOTAL PRICE
                mismatch[matches[i]] = price/reg_strip - 1 #RELATIVE MISPRICING OF INFLATION-LINKED STRIPS

            # CALUCULATE THE CROSS-SECTIONAL AVERAGE RELATIVE MISPRICING, AVERAGE MATURITY OF MATCHES AND NUMBER OF MATCHES
            average_mismatch = dict_average(mismatch)
            avg_mat = average_maturity(swapyears)
            n_of_matches = len(mismatch)

            # APPEND RESULTS TO VECTOR
            mastervec.append(mismatch)

    # WRITE TIME-SERIES OF AVERAGE MISPRICING TO OUTPUT-FILE
    write_results(mismatch)

main()

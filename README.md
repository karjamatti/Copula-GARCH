# ABOUT

This app simulates daily returns of a portfolio that consists of 4 asset class indices.
The simulation is based on the GARCH-Copula framework.

## REQUIREMENTS

The App is currently written so that it runs on MacOS.
If you wish to run it on other systems, please check that the path to the data file works.

A lot of the packages used in the script require compiling. This can get dicey, but some tips and tricks I can give are:

 - Install homebrew (availble from https://brew.sh/)
 - Install the required compilers (an easy way is to run the command: $ brew install gcc)
 - Check the "Makevars" -file in ~/.R/

The "Makevars" -file should be something along the lines of:

```
VER=-9 
CC=gcc$(VER)
CXX=g++$(VER)
CFLAGS=-mtune=native -g -O2 -Wall -pedantic -Wconversion
CXXFLAGS=-mtune=native -g -O2 -Wall -pedantic -Wconversion
FLIBS=-L/usr/local/Cellar/gcc/9.2.0/lib/gcc/9
```
where the number 9 etc. corresponds to your gcc version. (run the command "$ which gcc" to find out.)

## APP USER INSTRUCTIONS

An easy way to run the app, is to open the script in RStudio, and run all of the code.
This should eventually open a pop up window with the user interface.
Do not attempt to alter the code while the pop-up is open!

The script should automatically load, or install and load and load any required packages. however, do keep in mind that the packages might require compiling, which usually asks for a simple user-input (y/n). 
These inputs might mess up sourcing/running the code, so maybe try installing the required packages in the console first.

Typically, the user should only need to adjust or specify parameters that are accessible through the user interface of the app.
However, if the application shows an error messages, this might be due to incorrectly specified parameters in the script. (e.g. number of index return vectors per asset class)

The parameters most likely needing adjustment are at the beginning of the script for convenience.

## DEFAULT DATA SET

On Github, the **original** default dataset is not provided due to licensing reasons.
However, a **pseudo-dataset** based on the original with *randomization* is provided.

To ensure the functionality of the app, the data should be called 'dailydata.csv',
and contain trading dates in the first column (header 'Date'), and the returns of the following 7 indices/instruments:

	- MSCI WORLD (Equity)
	- MSCI Emerging Markets (Equity)
	- SP 500 (Equity)
	- US10Y (Gov Bond)
	- DE10Y (Gov Bond)
	- SP Investment Garde (Corp Bond)
	- SP InvestmentHigh Yield (Corp Bond)
	- MSCI World Real Estate (Real Estate)
	- MSCI US Real Estate (Real Estate)

## PROVIDING CUSTOM INDEX RETURNS AND UPDATING THE DEFAULT DATA SET

In order to yield accurate condtional forecasts, the return data should be updated daily. (Ironically enough, this is not the case for me, since I do not have access to the original data source any longer...)

When/if updating data, please remember to keep in mind:

	- Dates should be in YYYY-MM-DD format
	- Commas separate vector elements
	- Any removal or addition of columns to the data file should be specified in the script (more below)
	- Name the file dailyreturns.csv, or change the script

## SPECIFYING PARAMETERS IN THE SCRIPT

The most likely cause for adjusting parameters in the script is in case the user wants to provide their own return data.
In this case, the number of return indices in the adjusted data file should be specified, so that the user interface recognizes which asset classes the indices belong to.

Also, the number of simulated variates can be changed by adjusting the parameters at the beginning of the script.
Keep in mind, that while this speeds up the simulations, it reduces the convergence of the model fitting, and might yield surprisingly inaccurate forecasts.

## ODD-LOOKING DISTRIBUTIONS

It is very well possible that some index returns do not fit the GARCH-specfications that well.
It is also possible that the GARCH-fit might not be sufficient for parameter convergence!
In case of no convergence, you can try to specify a tolerance parameter in the garch fit, for example:

```r
specs <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)), # Srandard GARCH with order (1,1)
                                mean.model = list(armaOrder=c(1,1)), # ARMA(1,1)
				solver.control = list(tol = 1e-12),
                                distribution.model = "std") # Student t disturbances
 ```           
on line 167 of the application script

The correctness of the script is also not guaranteed! If you see something weird and cath a bug, please shoot me an email at karjamatti@gmail.com!

## DISCLAIMERS

The script is likely to contain errors, bad practices and other mishaps.
If you have any questions, please contact the email address found in this document.

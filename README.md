# ABOUT

This app simulates daily returns of a portfolio that consists of 4 asset class indices.
The simulation is based on the GARCH-Copula framework.
This app was submitted as a course project at the Master's level 'Portfolio Management' course
at the Aalto University School of Business, fall 2019.


## APP USER INSTRUCTIONS

An easy way to run the app, is to open the script in RStudio, and run all of the code.
This should eventually open a pop up window with the user interface.
Do not attempt to alter the code while the pop-up is open!

The location of the default return data file (dailyreturns.csv) should be automatically detected.
If this is not the case, please locate the file, and set the working directory manually in RStudio.

The script should automatically load, or install and load and load any required packages.

Typically, the user should only need to adjust or specify parameters that are accessible through the user interface of the app.
However, if the application shows an error messages, this might be due to incorrectly specified parameters in the script. (e.g. number of index return vectors per asset class)

The parameters most likely needing adjustment are at the beginning of the script for convenience.

## DEFAULT DATA SET

On Github, the default dataset is not provided due to licensing reasons
To replicate the functionality of the app, the data should be called 'dailydata.csv',
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

In order to yield accurate condtional forecasts, the return data should be updated daily.
When doing this, please remember to keep in mind:

	- Dates should be in YYYY-MM-DD format
	- Commas separate vector elements
	- Any removal or addition of columns to the data file should be specified in the script (more below)
	- Name the file dailyreturns.csv, or change the script

## SPECIFYING PARAMETERS IN THE SCRIPT

The most likely cause for adjusting parameters in the script is in case the user wants to provide his/her own return data.
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
on line 199 of the application script

The correctness of the script is also not guaranteed! If you see something weird and cath a bug, please shoot me an email at karjamatti@gmail.com!

## DISCLAIMERS

This submission is part of a school project, and should NOT be used in financial decision making!!!!
The outputs of the tool are purely illustrative and should not be taken as certain.
The script is written by a non-professional developer, and is likely to contain errors, bad practices and other mishaps.
The functionality of script is in no way guaranteed.
This submission is not meant to be distributed in exchange for any compensation.
If you have any questions, please contact the email address found in this document.

ALL RIGHTS RESERVED.
Matti Karjalainen, 2019.

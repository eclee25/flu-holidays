 #!/usr/bin/python

from datetime import date, datetime, timedelta
from itertools import product

## NOTE ##
# This code might not run if you use it directly. The purpose is to give you an example of how the  "SDIweek" function works so you can integrate it into your own code.

##############################################
def week_OR_processing(csv_incidence):
	''' Example import function when you have an incidence file with the following variables: season, week, agegroup code, ILI cases.
	'''	
	## import ILI data ##
	dict_ILI_week, dict_wk = {}, {}
	for row in csv_incidence: 
		week = row[1]
		Sun_dt = date(int(week[:4]), int(week[5:7]), int(week[8:]))
		wk, seas, _ = SDIweek(Sun_dt) # Thu date, season, wknum
		# dict_ILI_week[(week, agegroup code)] = ILI cases
		dict_ILI_week[(wk, str(row[2]))] = float(row[3])
		# dict_wk[week] = seasonnum
		dict_wk[wk] = seas

	return dict_wk, dict_ILI_week

##############################################
def SDIweek(datetimeWeek):
	''' Process Sunday date in SDI data to return Thursday date, correct season number, and week number, where season 0 is 1999-2000 flu season.
	'''
	Thu_date = datetimeWeek + timedelta(days=4) # Sun+4days=Thursday
	year, weeknum, _ = Thu_date.isocalendar() # returns yr, wknum, day
	# this if/else statement assigns the season number based on week number (weeks 40 to 39 belong to a single season)
	if weeknum >= 40:
		season = year+1-2000
	else:
		season = year-2000

	return Thu_date, int(season), weeknum
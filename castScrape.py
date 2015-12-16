## Title: IMDB sample database creator
## Author: Dax Gerts
## Date: 15 December 2016
## Description: used the OMDb API to rapidly build a clean JSON data sample of the IMDB database

import imdb
import csv
import re
import csv

ia = imdb.IMDb()

with open("clean10Kimdbids.csv") as f:
	reader = csv.reader(f)
	ids = map(tuple, reader)

with open('imdb_10K_cast_plus.csv','wb') as csvfile:
	idwriter = csv.writer(csvfile, delimiter=',')
	#for i in 1:len(ids):
	for i in range(1,len(ids)):
		#Assign temp string
		temp = str(ids[i])

		#Use regex to cut off ends of temp string
		temp = re.sub('\'\,\)$','',temp)
		temp = re.sub('\(\'tt','',temp)
		print(temp)

		#Identify movie by id
		movie = ia.get_movie(temp)

		#Attemp to retrieve producer list
		try:
			#Read producer as string
			producer = str(movie['producer'])

			#Repeatedly extract inline names
			producer = re.findall('_(.+?)_>',producer)

		except KeyError:
			#Fail to read producer, write "NA"
			producer = "NA"
	
		try:
			#Read cinema__ as string
			cinema = str(movie['cinematographer'])

			#Repeatedly extract inline names
			cinema = re.findall('_(.+?)_>',cinema)

		except KeyError:
			#Fail to read, write "NA"
			cinema = "NA"

		try:
			#Read composers as string
			composer = str(movie['composer'])

			#Repeatedly read inline names
			composer = re.findall('_(.+?_>',composer)
		except KeyError:
			#Fail to read, write "NA"
			composer = "NA"

		try:
			#Read costume-designer as tring
			costume = str(movie['costume-designer'])	

			#Repeatedly read inline names
			costume = re.findall('_(.+?_>',costume)
		except KeyError:
			#Fail to read, write "NA"
			costume = "NA"

		#Write producer string to file
		idwriter.writerow([temp,producer,cinema,composer,costume])

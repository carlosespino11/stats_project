import json
import urllib2
import csv
import time

IATA_codes = []

with open('airports_codes.csv', 'rU') as csvfile:
	reader = csv.DictReader(csvfile)
	for row in reader:
		IATA_codes.append(row['IATA_code'])

base_url="https://maps.googleapis.com/maps/api/geocode/json?address="

with open('airport_coordinates.csv','w') as csvfile:
	fieldnames = ['IATA_codes', 'latitude', 'longitude']
	writer = csv.DictWriter(csvfile, fieldnames = fieldnames)
	writer.writeheader()

	count = 0

	for j in range(0, (len(IATA_codes)/10 + 1)):
		tic = time.clock();
		for i in range(0,10):
			if (count == len(IATA_codes)):
				break

			url = base_url + IATA_codes[j*10+i] + '+airport'
			print(url)

			response = json.load(urllib2.urlopen(url))
			print(response['status'])

			if (response['status'] == "ZERO_RESULTS"):
				writer.writerow({'IATA_codes': IATA_codes[j*10+i], 'latitude': 'NA', 'longitude': 'NA'})
				count = count + 1
				print(count)
				continue

			latitude = response['results'][0]['geometry']['location']['lat']
			longitude = response['results'][0]['geometry']['location']['lng']
			writer.writerow({'IATA_codes': IATA_codes[j*10+i], 'latitude': latitude, 'longitude': longitude})

			count = count + 1
			print(count)
		toc = time.clock()
		time.sleep(1 - (toc-tic) + .5)

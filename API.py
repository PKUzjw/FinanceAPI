#!/usr/bin/python
# -*- coding: UTF-8 -*-
from flask import Flask, request
import numpy as np
import codecs, json 
from moduleallocate import allocation
app = Flask(__name__)

#accept POST
#parameter:theta, beta, gamma, age, numearner, numchild, income, \
#			muH, sigma_occu, sigma_sector, Medexp, mu_Y, stock, \
#			bond, deposit, househome, mortgagehome, houseINV, mortgageINV \
#			Htransfer, Hsupport, ReverseM, Cmean, Cbar, edufund, edufundQ, edufundyr
#notice:text format
#{"theta":theta, "beta":beta, "gamma":gamma, "age":age, "numearner":numearner, "numchild":numchild, \
# "income":income, "muH":muH, "sigma_occu":sigma_occu, "sigma_sector":sigma_sector, "Medexp":Medexp, \
# "mu_Y":mu_Y, "stock":stock, "bond":bond, "deposit":deposit, "househome":househome, "mortgagehome":mortgagehome, \
# "houseINV":houseINV, "mortgageINV":mortgageINV, "Htransfer":Htransfer, "Hsupport":Hsupport, "ReverseM":ReverseM, \
# "Cmean":Cmean, "Cbar":Cbar, "edufund":edufund, "edufundQ":edufundQ, "edufundyr":edufundyr}


#return data:apath, bpath, dpath, hpath, mpath, cpath, \
#	mapath, mbpath, mdpath, mhpath, mmpath, mcpath, \
#	napath, nbpath, ndpath, nhpath, nmpath, ncpath
#return data is in json format
# {'return_code': '200', 
# 'return_info': 'success', 
# 'result':{}
# }
@app.route("/API", methods=["POST"])
def getData():
	return_dict = {'return_code': '200', 'return_info': 'success', 'result': {}}
	if request.get_data() is None:
		return_dict['return_code'] = '404'
		return_dict['return_info'] = 'false'
		return json.dumps(return_dict, ensure_ascii = False)

	getData = request.get_data()
	getData = json.loads(getData)

	theta = getData.get('theta')
	beta = getData.get('beta')
	gamma = getData.get('gamma')
	age = getData.get('age')
	numearner = getData.get('numearner')
	numchild = getData.get('numchild')
	income = getData.get('income')
	muH = getData.get('muH')
	sigma_occu = getData.get('sigma_occu')
	sigma_sector = getData.get('sigma_sector')
	Medexp = getData.get('Medexp')
	mu_Y = getData.get('mu_Y')
	stock = getData.get('stock')
	bond = getData.get('bond')
	deposit = getData.get('deposit')
	househome = getData.get('househome')
	mortgagehome = getData.get('mortgagehome')
	houseINV = getData.get('houseINV')
	mortgageINV = getData.get('mortgageINV')
	Htransfer = getData.get('Htransfer')
	Hsupport = getData.get('Hsupport')
	ReverseM = getData.get('ReverseM')
	Cmean = getData.get('Cmean')
	Cbar = getData.get('Cbar')
	edufund = getData.get('edufund')
	edufundQ = getData.get('edufundQ')
	edufundyr = getData.get('edufundyr')
	apath, bpath, dpath, hpath, mpath, cpath, \
	mapath, mbpath, mdpath, mhpath, mmpath, mcpath, \
	napath, nbpath, ndpath, nhpath, nmpath, ncpath = allocation(theta, beta, gamma, age, numearner, numchild , income, muH, sigma_occu, sigma_sector, Medexp, mu_Y, stock, bond, deposit, househome, mortgagehome, houseINV, mortgageINV, Htransfer, Hsupport, ReverseM, Cmean, Cbar, edufund, edufundQ, edufundyr)
	return_dict['result']['apath'] = apath.tolist()
	return_dict['result']['bpath'] = bpath.tolist()
	return_dict['result']['dpath'] = dpath.tolist()
	return_dict['result']['hpath'] = hpath.tolist()
	return_dict['result']['mpath'] = mpath.tolist()
	return_dict['result']['cpath'] = cpath.tolist()
	return_dict['result']['mapath'] = mapath.tolist()
	return_dict['result']['mbpath'] = mbpath.tolist()
	return_dict['result']['mdpath'] = mdpath.tolist()
	return_dict['result']['mhpath'] = mhpath.tolist()
	return_dict['result']['mmpath'] = mmpath.tolist()
	return_dict['result']['mcpath'] = mcpath.tolist()
	return_dict['result']['napath'] = napath.tolist()
	return_dict['result']['nbpath'] = nbpath.tolist()
	return_dict['result']['ndpath'] = ndpath.tolist()
	return_dict['result']['nhpath'] = nhpath.tolist()
	return_dict['result']['nmpath'] = nmpath.tolist()
	return_dict['result']['ncpath'] = ncpath.tolist()
	return json.dumps(return_dict, ensure_ascii=False)

if __name__ == "__main__":
    app.run(host='0.0.0.0', port = 5000, debug=True)

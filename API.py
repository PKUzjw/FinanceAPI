from flask import Flask, request
import json
from moduleallocate import allocation
app = Flask(__name__)

#只接受POST方法访问
#输入的参数有：theta，beta，gamma，age，numearner，numchild，income，\
#			muH，sigma_occu，sigma_sector，Medexp，mu_Y，stock，\
#			bond，deposit，househome，mortgagehome，houseINV，mortgageINV \
#			Htransfer，Hsupport，ReverseM，Cmean，Cbar，edufund，edufundQ，edufundyr
#注意是以text的格式：
#{"theta":theta, "beta":beta, "gamma":gamma, "age":age, "numearner":numearner, "numchild":numchild, \
# "income":income, "muH":muH, "sigma_occu":sigma_occu, "sigma_sector":sigma_sector, "Medexp":Medexp, \
# "mu_Y":mu_Y, "stock":stock, "bond":bond, "deposit":deposit, "househome":househome, "mortgagehome":mortgagehome, \
# "houseINV":houseINV, "mortgageINV":mortgageINV, "Htransfer":Htransfer, "Hsupport":Hsupport, "ReverseM":ReverseM, \
# "Cmean":Cmean, "Cbar":Cbar, "edufund":edufund, "edufundQ":edufundQ, "edufundyr":edufundyr}


#返回的数据有:apath, bpath, dpath, hpath, mpath, cpath, \
#	mapath, mbpath, mdpath, mhpath, mmpath, mcpath, \
#	napath, nbpath, ndpath, nhpath, nmpath, ncpath
#返回的数据是以json的格式返回
# {'return_code': '200', 
# 'return_info': 'success', 
# 'result':{}
# }
@app.route("/index", methods=["POST"])
def getData():
	#默认的返回内容
	return_dict = {'return_code': '200', 'return_info': 'success', 'result': {}}
	#判断入参是否为空
	if request.get_data() is None:
		return_dict['return_code'] = '404'
		return_dict['return_info'] = 'false'
		return json.dumps(return_dict, ensure_ascii = False)

	#读取传入的参数
	getData = request.get_data()

	#传入的参数为bytes类型，需要转化为json
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
	return_dict['result']['apath'] = apath
	return_dict['result']['bpath'] = bpath
	return_dict['result']['dpath'] = dpath
	return_dict['result']['hpath'] = hpath
	return_dict['result']['mpath'] = mpath
	return_dict['result']['cpath'] = cpath
	return_dict['result']['mapath'] = mapath
	return_dict['result']['mbpath'] = mbpath
	return_dict['result']['mdpath'] = mdpath
	return_dict['result']['mhpath'] = mhpath
	return_dict['result']['mmpath'] = mmpath
	return_dict['result']['mcpath'] = mcpath
	return_dict['result']['napath'] = napath
	return_dict['result']['nbpath'] = nbpath
	return_dict['result']['ndpath'] = ndpath
	return_dict['result']['nhpath'] = nhpath
	return_dict['result']['nmpath'] = nmpath
	return_dict['result']['ncpath'] = ncpath
	return json.dumps(return_dict, ensure_ascii=False)

if __name__ == "__main__":
    app.run(host='0.0.0.0', port = 5590, debug=True)

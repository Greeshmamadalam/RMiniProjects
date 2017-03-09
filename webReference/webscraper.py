import requests
from html.parser import HTMLParser
from bs4 import BeautifulSoup
import nltk
from nltk import word_tokenize
import timeit
import re
import sys
import operator

with open('stop.txt') as f:
	stopwords = [line.rstrip('\n').strip() for line in f]

with open('kstop.txt') as f:
	kountablestopwords = [line.rstrip('\n').strip() for line in f]
def splitkey(word, unique = True):
	shortword = re.compile('[^0-9a-zA-Z@]+')
	word = shortword.sub(' ',word)
	' '.join(word.split())
	word = word.lower()
	if unique == False:
		words = word_tokenize(word)
	else:
		words = set(word_tokenize(word))
	word_list = [x for x in words if len(x) > 3]
	word_list = [x for x in word_list if x not in stopwords]
	return word_list
Keywords = [line.rstrip('\n') for line in open("keywords.txt", 'r')]
Helpwords = [line.rstrip('\n') for line in open("helpwords.txt", 'r')]
# Keywords = ["MUKARIVUZE VENANTIE","VENANTIE MUKARIVUZE","venantie@biopharmacia.rw","BIOPHARMACIA COMPANY LTD","VENANTIE MUKARIVUZE","250788304445"]
# Helpwords = ["BIOPHARMACIA Company Ltd., headquartered in Kigali, Rwanda, is an East African company dealing in Diagnostics, Consumable and Medical products. We are one of the East African’s development service delivery companies specialized in providing quality service in diagnostics, pharmaceutical and Medical products. In particular, we use our people’s experience of pharmacy, laboratory and biomedical expertise in our services. We are registered in Rwanda from 2011 under the trading name of BIOPHARMACIA Company Ltd.","Jakaranda", "Estate 2020,KG 383 ST No:9","Nyarugenge","Kigali","kigali City","Rwanda","PO Box 2513","Kigali","Gasabo","Rwanda"]
Keywords = list(map(splitkey,Keywords))
Keywords = list(set([val for sublist in Keywords for val in sublist]))
for x in Keywords:
	if x in kountablestopwords:
		Keywords.remove(x)
Helpwords = list(map(splitkey,Helpwords))
Helpwords = list(set([val for sublist in Helpwords for val in sublist]))
linkedin = list()
# r1 = re.compile("$.pdf")
k=0;
def getscore(url):
	print(url)
	# global r1
	try:
		r = requests.get(url)
		soup = BeautifulSoup(r.text,"html.parser")
		to_extract = soup.findAll('script')
		to_extract+=soup.findAll('style')
		for item in to_extract:
			item.extract()
		text = soup.get_text()
		# print(text)
	except requests.exceptions.RequestException as e:
		print("exception")
		# print(" ********** SCORE *********")
		# print(0)
		return(0)	
	
	text = splitkey(text, False)
	if not text:
		print("forbidden")
		return(0)
		# if url == "https://twitter.com/biopharmaciarwa":
		# 	print(" r")
		# 	print(r.text)
		# 	print(soup)
		# 	print(text)
	# # keyword extraction and tokenizing

	global Keywords
	global Helpwords
	# print(" ********** keykeys*********")
	# print(Keywords)
	# print(" ********** helpkeys*********")
	# print(Helpwords)

	# Keywords = getkeywords()
	# Helpwords = gethelpwords()
	texter = " ".join(text)
	keycounts={}
	# print(" ********** counts before *********")
	# print(keycounts)	
	# print(texter)
	for word in Keywords:
		if texter.count(word) is not 0:
			keycounts[word] = texter.count(word)
	# print(keycounts)
	# print(sum(keycounts.values()))

	# # 	# print(url_to_scrape+"//*&&&&&*//0")
	# # 	# print(" ********** no keys*************")
	# # 	# print(keycounts)
	# # 	# print(" ********** SCORE *********")

	urltext = " ".join(splitkey(url))
	for word in Keywords:
		if urltext.count(word) is not 0:
			keycounts[word] = (urltext.count(word)+(keycounts[word] if word in keycounts.keys() else 0))*100
	# print(" ********** check urls*********")
	# print(keycounts)
	# print(sum(keycounts.values()))
	if sum(keycounts.values()) is 0:
		# print(url_to_scrape+"//*&&&&&*//0")
		# print(" ********** no keys*************")
		# print(keycounts)
		# print(" ********** SCORE *********")
		return(0)
	for word in Helpwords:
		if word not in Keywords:
			if texter.count(word) is not 0:
				keycounts[word] = texter.count(word)
				keycounts[word] = (urltext.count(word)+(keycounts[word] if word in keycounts.keys() else 0))*10
	# print(" ********** check helps *********")
	# print(keycounts)
	print(" ********** SCORE *********")
	print(sum(keycounts.values()))
	return(sum(keycounts.values()))
with open('input.txt') as f:
	fa = [line.rstrip('\n') for line in f]
urlranks={}
for url_to_scrape in fa:
	score = getscore(url_to_scrape)
	urlranks[url_to_scrape]=score
sorted_urlranks = sorted(urlranks.items(), key=operator.itemgetter(1) )
sorted_urlranks.reverse()
print(sorted_urlranks)
with open('output.txt','w') as f:
	for x in sorted_urlranks:
		f.write(str(x[0])+"\t"+str(x[1])+"\n")


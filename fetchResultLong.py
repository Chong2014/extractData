#!/usr/bin/python
#encoding: utf-8

import MySQLdb as mdb
import _mysql
import sys

def DatabaseConnection():
    return mdb.connect('localhost', 'root', 'mysql', 'experiments', charset='utf8')

def main():    
    reload(sys)
    sys.setdefaultencoding('utf-8')
    con = DatabaseConnection()
    cur = con.cursor()
    cmd = """select word from ExperimentDefinition e inner join results r where e.ID = r.sentenceId AND Demonstrative = 'DEM-' AND RCtype = 'ACTIVE' AND Plausibility = 'POSS-REL' AND MainFill = 'FILL' AND WordNumber ='?';"""
    cur.execute(cmd)
    fname = "test.txt"
    file = open(fname, "w")
    count = 0
    while (1):
    	row = cur.fetchone()
    	if row == None: 
    		break
    	print>>file, str(row).decode('utf8')
    	count = count + 1
    print count
    cur.close()
    con.close()
main()
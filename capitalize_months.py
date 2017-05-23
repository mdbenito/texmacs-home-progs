#!/usr/bin/env python
# coding: utf-8

import sys

def capitalize_months(path_to_bib):
    """ Capitalize month names in a bibtex file."""
    def capitalize(line):
        if not line.startswith('\tmonth = '):
            return line
        else:
            split = line.split('\tmonth = ')
            res = '\tmonth = ' + split[1].title()
            return res
    
    with open(path_to_bib) as f:
        lines = f.readlines()    
    lines = list(map(capitalize, lines))
    with open(path_to_bib, 'w') as f:
        f.writelines(lines)
        
if __name__ == '__main__':
	try:
		path_to_bib = sys.argv[1]
		capitalize_months(path_to_bib)
	except:
		print('Error')
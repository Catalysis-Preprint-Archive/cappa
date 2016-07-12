from mako.template import Template
import bibtexparser
import os
import urllib2

template_string = '''
<html>
<head>
</head>
<body>
<h1>
Available Preprints
</h1>
<table border = "2" cellspacing = "0" cellpadding = "6">
    <thead>
        <th>Preprint</th>
        <th>DOI</th>
        <th>Repo</th>
        <th>Citation</th>
    </thead>
    <tbody>
    % for recipe in recipes: 
        <tr>
        <td><a href = "${'preprints/%s.html'%filenames[recipe['repo']]}">${recipe['ID']}</a></td>
        <td><a href = "${recipe['link']}">doi</a></td>
        <td><a href = "${recipe['repo']}">${recipe['repo']}</a></td>
        <td>citation to come</td>
        </tr>
    % endfor
    </tbody>
</table>
</body>
</html>
'''

def read_bibtex_file(path):
    with open(path) as bibtex_file:
        contents = bibtex_file.read()
    return contents

def check_valid_connection(url):
    try:
        r = urllib2.urlopen(url)
    except urllib2.URLERROR as e:
        r = e
    return r.code != 404

def make_index():
    recipes_list = []
    filenames = {}
    for filename in os.listdir('recipes'):
        path = 'recipes/' + filename
        file_contents = read_bibtex_file(path)
        #returns a list of dictionaries for all bibtex entries in file
        parsed_file = bibtexparser.loads(file_contents)
        for entry in parsed_file.entries:
            if check_valid_connection(entry['repo']):
                recipes_list.append(entry)
                filenames[entry['repo']] = filename
    template = Template(template_string)
    with open('html/index.html', 'wt') as f:
        f.write(template.render(recipes = recipes_list, filenames = filenames))

if __name__ == "__main__":
    make_index()


from mako.template import Template
import bibtexparser
import os
import utils

template_string = '''
<html>
<head>
</head>
<body>
<h1>
Available Preprints

<script>
  (function() {
    var cx = '002533177287215655227:3hwzazzcvog';
    var gcse = document.createElement('script');
    gcse.type = 'text/javascript';
    gcse.async = true;
    gcse.src = (document.location.protocol == 'https:' ? 'https:' : 'http:') +
        '//cse.google.com/cse.js?cx=' + cx;
    var s = document.getElementsByTagName('script')[0];
    s.parentNode.insertBefore(gcse, s);
  })();
</script>
<gcse:search></gcse:search>


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


def make_index():
    recipes_list = []
    filenames = {}
    for filename in os.listdir('recipes'):
        path = 'recipes/' + filename
        file_contents = utils.read_bibtex_file(path)
        #returns a list of dictionaries for all bibtex entries in file
        parsed_file = bibtexparser.loads(file_contents)
        for entry in parsed_file.entries:
            if utils.check_valid_connection(entry['repo']):
                recipes_list.append(entry)
                filenames[entry['repo']] = filename
    template = Template(template_string)
    with open('html/index.html', 'wt') as f:
        f.write(template.render(recipes = recipes_list, filenames = filenames))

if __name__ == "__main__":
    make_index()


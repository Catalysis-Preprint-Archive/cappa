from mako.template import Template
import bibtexparser
import os
import urllib2

template_string = '''<html>
<head>
<script id="altmetric-embed-js" src="https://d1bxh8uas1mnw7.cloudfront.net/assets/altmetric_badges-5526d9ad7f25d925d839db2002a44179.js"></script>
</head>
<body>
<h1>
    "${bibtex_entry['title']} ("
    <a href = "../../${path}">recipe</a>
    ")"
</h1>
<a href = "../index.html">Home</a>
<br>
<h2>
<a href = "${bibtex_entry['link']}">${bibtex_entry['title']}</a>
</h2>
<table>
    <tbody>
        <tr>
            <td>
            <a href = "${bibtex_entry['repo']}">repo</a>
            <div data-badge-type="medium-donut" class="altmetric-embed" 
            data-badge-details="right" data-doi="10.1103/PhysRevLett.93.156801" 
            data-uuid="0361e5a0-7a27-4142-2eb3-bef6286772b4">
            <a href="https://www.altmetric.com/details.php?domain=catalysis-
            preprint-archive.github.io&amp;doi=10.1103%2Fphysrevlett.93.156801" 
            target="_self" style="cursor: pointer; display: inline-block; width: 
            120px; height: 120px; background-image: url(&quot;https://altmetric-
            badges.a.ssl.fastly.net/?size=120&amp;score=?&amp;types=????????&amp
            ;style=donut&quot;);"> </a></div>
            <pre>
            ${citation}
            </pre>
            </td>
        </tr>
    </tbody>
</table>
</body>
</html>            
'''
def check_valid_connection(url):
    try:
        r = urllib2.urlopen(url)
    except urllib2.URLERROR as e:
        r = e
    return r.code != 404

def read_bibtex_file(path):
    with open(path) as bibtex_file:
        contents = bibtex_file.read()
    return contents

def get_html_files():
    for filename in os.listdir('recipes'):
        path = 'recipes/' + filename
        file_contents = read_bibtex_file(path)
        #returns a list of dictionaries for all bibtex entries in file
        parsed_file = bibtexparser.loads(file_contents)
        for entry in parsed_file.entries:
            template = Template(template_string)
            if check_valid_connection(entry['repo']):
                with open('html/preprints/%s.html'%filename, 'wt') as f:
                    f.write(template.render(bibtex_entry = entry, path = path,
                    citation = file_contents))

if __name__ == '__main__':
    get_html_files()


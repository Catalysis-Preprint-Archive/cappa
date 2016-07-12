"Simple utilities."

import urllib.request

def read_bibtex_file(path):
    """Read the file at path and return the contents."""
    with open(path) as bibtex_file:
        contents = bibtex_file.read()
    return contents


def check_valid_connection(url):
    try:
        r = urllib.request.urlopen(url)
    except urllib.request.URLERROR as e:
        r = e
    return r.code != 404

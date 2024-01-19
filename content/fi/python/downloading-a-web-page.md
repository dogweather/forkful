---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Verkkosivun lataaminen on prosessi, jossa tiedot tuotetaan koneellesi verkkosivulta. Ohjelmoijat tekevät tämän tietojen kaapimiseksi tai offline-käyttöä varten.

## Kuinka:

Pythonissa voit käyttää `requests`-kirjastoa verkkosivun lataamiseen. Tässä on esimerkki:

```Python
import requests

url = "http://www.python.org"
response = requests.get(url)

print(response.text)
```

Kun suoritat yllä olevan koodin, saat outputin, joka näyttää Python.org-verkkosivun HTML-koodin.

## Syvempi sukellus:

Historiallisesti verkkosivun lataaminen on ollut välttämätöntä, jotta voimme käyttää tai tutkia sivun tietoja. Pythonin `requests`-kirjasto on suosituin työkalu tähän, mutta on muitakin vaihtoehtoja, kuten `urllib` ja `httplib`.

Tässä on esimerkki verkkosivun lataamisesta käyttämällä `urllib`-kirjastoa:

```Python
from urllib.request import urlopen

url = "http://www.python.org"
response = urlopen(url)

print(response.read().decode())
```

## Katso myös:

- Python requests dokumentaatio: http://docs.python-requests.org/en/latest/
- urllib — URL handling modules: https://docs.python.org/3/library/urllib.html
- http.client - HTTP protocol client: https://docs.python.org/3/library/http.client.html
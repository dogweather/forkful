---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse HTML betyr å nøste opp innholdet i en webside, slik at det gir mening for datamaskinen. Programmerere parser HTML for å trekke ut bestemt informasjon, som linker eller tekst, fra nettsider.

## Hvordan Gjøre Det:
Her er et enkelt eksempel med bruk av `BeautifulSoup`, en populær Python-bibliotek for web skraping. For å begynne, installere `beautifulsoup4` og `requests` pakker:

```Python 
pip install beautifulsoup4 requests
```

Etter installasjonen, her er hvordan å parse en enkel HTML:

```Python
from bs4 import BeautifulSoup
import requests

html_text = requests.get('https://www.google.com').text
soup = BeautifulSoup(html_text, 'html.parser')

print(soup.prettify())
```

Når du kjører dette skriptet, vil det vise HTML-strukturen til Google's hjemmeside i terminalen.

## Dybdeplunge
HTML-parsing har eksistert siden HTML ble oppfunnet tidlig på 90-tallet. Mens `BeautifulSoup` er en populær løsning, er det mange alternativer som `lxml`, `html.parser`, og til og med regulære uttrykk (men disse frarådes for komplekse HTML-dokumenter på grunn av deres begrensninger).

Parsing HTML kan være mer eller mindre komplekst, avhengig av koden du arbeider med. For eksempel, noen nettsteder kan benytte JavaScript til å laste inn data, noe som kan komplisere den vanlige parsingprosessen.

## Se Også
 - BeautifulSoup Dokumentasjon: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
 - Python's `lxml` bibliotek: https://lxml.de/tutorial.html
 - HTML parsing med regulære uttrykk i Python: https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags
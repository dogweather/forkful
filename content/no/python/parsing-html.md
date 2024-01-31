---
title:                "Analyse av HTML"
date:                  2024-01-20T15:33:21.863554-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML betyr å tolke HTML-koden for å trekke ut data, som tekst, linker eller strukturinfo. Programmører gjør det for å automatisere innsamling eller behandling av info fra nettsider.

## How to:
For å parse HTML i Python, kan du bruke BeautifulSoup biblioteket. La oss se på et eksempel:

```Python
from bs4 import BeautifulSoup
import requests

# Hent siden og lag en BeautifulSoup objekt
url = "https://eksempel.no"
side_innhold = requests.get(url).content
soup = BeautifulSoup(side_innhold, 'html.parser')

# Finn alle paragrafer
paragrafer = soup.find_all('p')
for para in paragrafer:
    print(para.get_text())

# Finn en spesifikk klasse
spesifikk_klasse = soup.find_all('div', class_='min-klasse')
for elem in spesifikk_klasse:
    print(elem.get_text())
```

Output vil vise teksten til alle paragrafer, og deretter teksten til alle elementer med klassen `min-klasse`.

## Deep Dive
Parsing av HTML har vært nødvendig siden nettets begynnelse for å gjøre informasjonen weben tilgjengelig for programmer. I historien har verktøy som `regex` blitt brukt, men de er ofte upålitelige på kompleks HTML. BeautifulSoup, lansert i 2004, sammen med andre biblioteker som `lxml` og `html.parser`, gir en mer robust løsning.

Alternativer til BeautifulSoup kan være `lxml` eller `html5lib` som parser. De kan være raskere eller bedre til å håndtere dårlig formatert HTML, men BeautifulSoup er kjent for sin brukervennlighet og fleksibilitet.

Ved implementering av HTML-parsing, håndterer programmerere ofte 'web scraping', som innebærer å extrahere data fra nettsteder. Det er viktig å være oppmerksom på lovligheten rundt dette samt nettstedets `robots.txt` for å respektere brukervilkårene.

## See Also
- BeautifulSoup dokumentasjon: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Requests biblioteket dokumentasjon: https://requests.readthedocs.io/
- Lxml parser: https://lxml.de/
- HTML5lib parser: https://html5lib.readthedocs.io/en/latest/

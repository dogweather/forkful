---
title:                "Analysering av HTML"
html_title:           "Python: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hva er HTML parsing, og hvorfor er det relevant for en Python programhopper? HTML parsing er prosessen med å analysere og hente ut informasjon fra HTML-kode for å manipulere og bruke i våre programmer. Dette er spesielt nyttig når vi ønsker å hente ut spesifikke data fra nettsider, for eksempel priser, produktbeskrivelser, og annen informasjon som vi kan bruke til å bygge våre egne applikasjoner.

## Hvordan

For å kunne parse HTML med Python, trenger vi et modul som hjelper oss med å håndtere HTML-koden og hente ut data etter ønske. Et populært modul for dette formålet er `BeautifulSoup`, som kan installeres ved å kjøre `pip install beautifulsoup4` i terminalen. La oss se på et eksempel der vi ønsker å hente ut tittel og pris for et produkt fra en nettbutikk:

```Python
from bs4 import BeautifulSoup
import requests

url = 'https://www.nettbutikk.no/produkt/12345'
response = requests.get(url)
soup = BeautifulSoup(response.text, 'html.parser')

title = soup.find('h1').text # henter ut tittelen fra <h1> taggen
price = soup.find('div', {'class': 'product-price'}).text # henter ut prisen fra <div class="product-price">

print('Produkt: ' + title)
print('Pris: ' + price)
```

Dette eksempelet viser noen grunnleggende funksjoner i BeautifulSoup, som `find()` og `text`, som vi bruker for å finne og hente ut elementer og tekst fra HTML-koden. Vi kan også bruke `find_all()` for å finne alle forekomster av et bestemt element i HTML-koden.

## Dypdykk

HTML parsing kan være en kompleks prosess, spesielt når vi må håndtere store og komplekse nettsider. En viktig del av å lykkes med HTML parsing er å forstå hvordan HTML-koden er strukturert og hvordan vi kan identifisere og hente ut de dataene vi ønsker. Dette kan ta litt tid og krever ofte at vi må prøve og feile for å finne den beste måten å håndtere den spesifikke nettstedet på.

En annen viktig ting å huske på er at HTML-koden på nettsider kan endre seg ofte, og dette kan påvirke vår parsing. Derfor er det viktig å regelmessig sjekke om koden har endret seg, og eventuelt justere koden vår for å fortsatt kunne hente ut dataene vi trenger.

## Se også

- [BeautifulSoup dokumentasjon](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Lær mer om HTML-koding](https://www.w3schools.com/html/)
- [Les om web scraping og etiske retningslinjer](https://www.knightlab.northwestern.edu/2014/03/13/a-beginners-guide-to-collecting-and-mapping-twitter-data-using-python-and-apis/)
---
title:                "Laste ned en nettside"
html_title:           "Python: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Nedlasting av en nettside er prosessen med å få tak i all informasjonen som utgjør nettsiden og lagre den på datamaskinen din. Dette er en vanlig oppgave for programmerere fordi det lar dem hente data fra ulike nettsider og bruke disse til å utvikle applikasjoner og automatisere oppgaver.

## Slik gjør du det:
Bruk ```Python requests``` biblioteket for å laste ned en nettside ved å bruke følgende kode:
```
import requests
response = requests.get("https://www.example.com")
print(response.text) 
```
Dette vil skrive ut alt innholdet på nettsiden i tekstformat. Du kan også bruke ```BeautifulSoup``` for å analysere eller søke gjennom det nedlastede innholdet.

## Dypere dykk:
Lignende alternativer for nedlasting av nettsider inkluderer ```urllib``` og ```urllib2```, men requests er den mest brukte løsningen på grunn av dens brukervennlighet og omfattende støtte fra utviklermiljøer. Det finnes også spesialiserte biblioteker som Scrapy for å utføre web scraping oppgaver.

## Se også:
- Offisiell requests dokumentasjon: https://requests.readthedocs.io/en/master/
- Beautiful Soup dokumentasjon: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Scrapy dokumentasjon: https://docs.scrapy.org/en/latest/
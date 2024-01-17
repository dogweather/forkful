---
title:                "Å sende en http-forespørsel"
html_title:           "Python: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Når vi programmerer, må vi noen ganger kommunisere med andre datamaskiner og servere for å hente informasjon eller utføre handlinger. Dette kalles å sende en HTTP-forespørsel. Det er en vanlig og viktig oppgave for mange programmer.

## Slik gjør du det:
For å sende en HTTP-forespørsel i Python, kan du bruke biblioteket "requests". Først importerer du biblioteket:
```
import requests
```
Deretter kan du bruke funksjonen `get()` for å sende en GET-forespørsel. For eksempel:
```
response = requests.get('https://www.google.com')
```
Dette vil sende en forespørsel til Google sin nettside og lagre svaret i variabelen `response`. Du kan deretter hente informasjon fra svaret, for eksempel statuskoden til forespørselen (200 betyr suksess) og innholdet i svaret (HTML-koden til nettsiden). For eksempel:
```
print(response.status_code)
print(response.content)
```
Dette vil gi deg følgende utput:
```
200
b'<!doctype html>\n<html ... '
```

## Dypdykk:
Å sende HTTP-forespørsler har vært en viktig del av webutvikling siden starten av internett. Det finnes flere alternative måter å sende slike forespørsler, som for eksempel "urllib" og "httplib". Men "requests" er den mest populære og anbefalte måten å gjøre det på i Python. Det finnes også avanserte teknikker for å konfigurere og håndtere HTTP-forespørsler i mer kompliserte programmer.

## Se også:
- [Requests dokumentasjon](https://requests.readthedocs.io/en/master/)
- [W3Schools HTTP tutorial](https://www.w3schools.com/whatis/whatis_http.asp)
- [Enkel guide til HTTP-forespørsler](https://krakenservices.github.io/kraken-rest/docs/http.html)
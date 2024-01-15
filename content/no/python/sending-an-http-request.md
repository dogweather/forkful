---
title:                "Sende en http-forespørsel"
html_title:           "Python: Sende en http-forespørsel"
simple_title:         "Sende en http-forespørsel"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med å sende en HTTP-forespørsel? Vel, det er en nødvendig del av å lage nettapplikasjoner og bruke API-er. Det er også en måte å kommunisere med andre servere og få tilgang til data og ressurser på nettet.

## Hvordan gjøre det

For å sende en HTTP-forespørsel i Python, kan du bruke innebygde biblioteker som urllib eller requests. Her er et eksempel på å sende en GET-forespørsel til et nettsted:

```Python
import requests

response = requests.get("https://www.example.com")
print(response.text)
```

Dette koden bruker requests-biblioteket for å sende en GET-forespørsel til nettadressen "https://www.example.com". Deretter skriver den ut innholdet i forespørselen ved hjelp av response.text-metoden. Det er også mulig å sende POST-forespørsler eller legge til spesifikke parametere og headers i forespørselen.

## Dykk dypere

Når du sender en HTTP-forespørsel, sender du en forespørsel til en spesifikk URL med en bestemt metode (for eksempel GET, POST, PUT, DELETE). Denne forespørselen sendes til en server, som svarer med en responskode og eventuelle data eller ressurser som ble forespurt. Det finnes forskjellige metoder og HTTP-statuskoder som angir forskjellige handlinger og svar, og det er viktig å forstå disse når du sender en forespørsel.

## Se også

- [Offisiell dokumentasjon for Python urllib-biblioteket](https://docs.python.org/3/library/urllib.html)
- [Offisiell dokumentasjon for Python requests-biblioteket](https://docs.python-requests.org/en/master/)
- [HTTP-forespørsler i Python: En praktisk guide](https://realpython.com/python-requests/)
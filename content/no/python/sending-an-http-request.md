---
title:                "Python: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å sende HTTP-forespørsler er en essensiell del av å kommunisere med nettbaserte tjenester. Det lar deg få tilgang til og hente informasjon fra ulike nettsider og applikasjoner. Det er også en viktig del av web scraping og automatisering av oppgaver på nettet.

## Hvordan Du Gjør Det

For å sende en HTTP-forespørsel i Python, må du først importere "requests" biblioteket. Deretter kan du bruke funksjonen "get()" for å sende en GET-forespørsel til en URL. For eksempel:

```Python
import requests

response = requests.get("https://www.python.org")

print(response.status_code)
```

Dette vil hente innholdet på python.org-siden og skrive ut statuskoden til den HTTP-forespørselen, som vanligvis vil være 200 hvis alt går bra. Du kan også få tak i informasjonen fra responsen, for eksempel HTML-koden, ved å bruke "response.text" eller konvertere den til en JSON-struktur ved å bruke "response.json()".

Du kan også sende POST-forespørsler ved å bruke funksjonen "post()" og inkludere dataene du ønsker å sende som et argument. For eksempel:

```Python
data = {"navn": "Jan", "alder": 25}

response = requests.post("https://www.examplesite.com/register", data=data)

print(response.text)
```

Dette vil sende en POST-forespørsel til examplesite.com sin register-endepunkt, med navn og alder som dataene som skal sendes. Du kan deretter bruke "print" til å få tilbake eventuelle svar fra nettstedet.

## Dypere Dykk

Å sende HTTP-forespørsler innebærer å samhandle med API-er, og kan være nyttig for å automatisere oppgaver på nettet. Du kan også bruke Python-biblioteker som "BeautifulSoup" for å analysere og trekke ut informasjon fra HTML-dokumenter som er hentet ved hjelp av HTTP-forespørsler.

Når du jobber med HTTP-forespørsler, er det også viktig å være klar over sikkerhet. Pass på å aldri sende sensitiv informasjon gjennom HTTP, da det ikke er en sikker protokoll. I stedet bør du bruke HTTPS for å sikre at dataene dine er kryptert under overføring.

## Se Også

- [Offisiell dokumentasjon for Requests- biblioteket](https://requests.readthedocs.io/en/master/)
- [BeautifulSoup-dokumentasjon](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [En guide til web scraping med Python](https://realpython.com/python-web-scraping-practical-introduction/)
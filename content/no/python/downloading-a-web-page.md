---
title:                "Å laste ned en nettside"
html_title:           "Python: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor:
Det å laste ned en nettside ved hjelp av Python kan være nyttig for å hente informasjon fra internett, som for eksempel tekst og bilder, til å utføre videre analyser eller bruke i et prosjekt.

## Hvordan:
For å laste ned en nettside i Python kan du bruke biblioteket "requests". Først må du importere biblioteket ved å legge til følgende linje øverst i koden din:

```Python
import requests
```

Deretter kan du bruke funksjonen "get" for å hente nettsiden du ønsker. Du må spesifisere nettsidens URL som et argument til funksjonen. Her er et eksempel på hvordan koden kan se ut:

```Python
# Importerer biblioteket
import requests

# Henter nettsiden
response = requests.get("https://www.example.com")

# Sjekker om nettsiden ble hentet suksessfullt
if response.status_code == 200:
  print("Nettsiden ble lastet ned!")
else:
  print("Noe gikk galt. Statuskode:", response.status_code)
```

Output vil være enten "Nettsiden ble lastet ned!" eller "Noe gikk galt. Statuskode: XX", hvor XX vil være en nummerisk verdi som angir eventuelle feilmeldinger.

Du kan også bruke funksjonen "content" til å hente innholdet på nettsiden som en tekststreng:

```Python
# Henter innholdet på nettsiden
page_content = response.content

# Skriver ut det første avsnittet på nettsiden
print(page_content.split("<p>")[1].split("<")[0])
```

Output vil være det første avsnittet på nettsiden du lastet ned.

## Dypdykk:
Requests-biblioteket har mange flere funksjoner og muligheter for å tilpasse og behandle nettverksforespørsler i Python. Du kan for eksempel bruke funksjonen "post" for å sende data til en nettside, eller bruke "headers" til å spesifisere ulike parametere som ønsket filformat eller språk. Det er også mulig å bruke denne metoden til å logge på en nettside ved å sende innloggingsinformasjon som data. Besøk dokumentasjonen til biblioteket for å lære mer om alle mulighetene det har å tilby.

## Se også:
- [Requests dokumentasjon](https://docs.python-requests.org/en/master/)
- [W3Schools How to use the requests Library in Python](https://www.w3schools.com/python/module_requests.asp)
- [RealPython Making HTTP Requests in Python using Requests](https://realpython.com/python-requests/)
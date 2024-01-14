---
title:                "Python: Parsing html"
simple_title:         "Parsing html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/parsing-html.md"
---

{{< edit_this_page >}}

# Hvorfor parse HTML i Python?

Som en programmerer, kan du komme over en situasjon hvor du trenger å få informasjon fra en nettside. Dette kan være for å lage en nettleser eller for å samle data til analyse. I slike tilfeller er det nyttig å kunne parse HTML-koden til nettsiden for å få den nødvendige informasjonen.

## Slik gjør du det med Python

Python er et kraftig programmeringsspråk som gjør det enkelt å parse HTML. Det finnes flere programmeringsbiblioteker som du kan bruke til dette formålet, men vi vil fokusere på "BeautifulSoup" som er en populær løsning blant utviklere.

Først må du installere BeautifulSoup ved å skrive følgende kommando i terminalen:

```Python
pip install beautifulsoup4
```

Etter at du har installert Beautifulsoup, kan du komme i gang med å parse HTML. Først må du importere biblioteket ved å legge til følgende linje i toppen av filen din:

```Python
from bs4 import BeautifulSoup
```

Deretter kan du bruke BeautifulSoup til å hente informasjon fra en nettside. La oss si at du vil hente informasjon om været fra nettstedet "yr.no". Først må du importere "requests" biblioteket som gjør det mulig å hente innhold fra en nettside:

```Python
import requests
```

Nå kan du bruke "get" metoden i "requests" for å hente innholdet på nettsiden og lagre det i en variabel:

```Python
page = requests.get("https://www.yr.no/sted/Norge/Oslo/Oslo/Oslo/")
```

Deretter kan du bruke BeautifulSoup til å parse HTML-en og lagre det i en variabel:

```Python
soup = BeautifulSoup(page.content, 'html.parser')
```
Til slutt kan du bruke BeautifulSoup-funksjoner og CSS-selektorer for å finne og hente ut den nødvendige informasjonen fra nettsiden. For eksempel, hvis du vil hente ut værvarselet for de neste tre dagene, kan du bruke følgende kode:

```Python
forecast = soup.find(class_="daily-weather-list")
days = forecast.find_all(class_="daily-weather-list__item-time")
weather = forecast.find_all(class_="daily-weather-list__item-temperature")
```

Dette vil gi deg dagene og været for hver dag i en liste. Du kan deretter bruke en løkke for å skrive ut denne informasjonen på en ryddig måte.

## Dypdykk i parsing av HTML

Som du kan se, kan Beautifulsoup gjøre det enkelt å hente informasjon fra nettsider ved hjelp av HTML-parsing. Det er også mulig å bruke BeautifulSoup til å finne og hente ut informasjon basert på spesifikke kriterier, for eksempel bestemte tagger eller attributter. Dette gjør det til et veldig allsidig verktøy for parsing av HTML.

Det er også verdt å nevne at Beautifulsoup også støtter å jobbe med både HTML og XML, noe som gjør det til et enda mer kraftig verktøy for å arbeide med nettinnhold.

# Se også

- [BeautifulSoup dokumentasjon](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Requests dokumentasjon](https://requests.readthedocs.io/en/master/)
- [HTML og CSS tutorial på W3Schools](https://www.w3schools.com/html/default.asp)
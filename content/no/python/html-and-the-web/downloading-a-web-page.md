---
aliases:
- /no/python/downloading-a-web-page/
date: 2024-01-20 17:45:00.297860-07:00
description: "Nedlasting av en nettside betyr \xE5 hente HTML-koden slik at du kan\
  \ se eller bearbeide den lokalt. Programmerere gj\xF8r dette for \xE5 analysere\
  \ innhold, sjekke\u2026"
lastmod: 2024-02-18 23:08:53.523270
model: gpt-4-1106-preview
summary: "Nedlasting av en nettside betyr \xE5 hente HTML-koden slik at du kan se\
  \ eller bearbeide den lokalt. Programmerere gj\xF8r dette for \xE5 analysere innhold,\
  \ sjekke\u2026"
title: Nedlasting av en nettside
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Nedlasting av en nettside betyr å hente HTML-koden slik at du kan se eller bearbeide den lokalt. Programmerere gjør dette for å analysere innhold, sjekke tilgjengelighet eller hente data.

## Hvordan gjøre det:
For å laste ned en nettside i Python, bruk `requests`-biblioteket:

```python
import requests

url = 'https://www.example.com'
response = requests.get(url)

# Sjekk om forespørselen var vellykket
if response.status_code == 200:
    html_content = response.text
    print(html_content)
else:
    print('Nettsiden kunne ikke lastes ned. Statuskode:', response.status_code)
```

Eksempel på utdata:
```
<!doctype html>
<html>
<head>
    <title>Eksempel Domene</title>
</head>
<body>
    <p>Dette er et eksempel på en nettside.</p>
</body>
</html>
```

## Dypdykk:
Historisk har web-skraping blitt gjort med verktøy som `curl` og `wget`. I Python-verdenen brukes ofte biblioteker som `requests` for enkel nedlasting og `BeautifulSoup` eller `lxml` for parsing. Når du laster ned nettsider, må du vurdere både lovlighet og etikett, som for eksempel å respektere `robots.txt` og ikke overbelaste servere.

Alternativer for `requests` kan være `http.client`, som er mer lavnivå, eller `urllib`, som følger med Python, men er mindre brukervennlig. For asynkron nedlasting kan `aiohttp` være nyttig.

En viktig implementeringsdetalj er håndtering av nettverksfeil og tidsavbrudd. Det er også viktig å sette riktig `User-Agent` header for å identifisere forespørselen korrekt til webserveren.

## Se Også:
- Requests-dokumentasjon: https://requests.readthedocs.io/en/master/
- BeautifulSoup-dokumentasjon: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Lxml-dokumentasjon: https://lxml.de/
- Håndtering av `robots.txt`: https://pypi.org/project/robotparser/

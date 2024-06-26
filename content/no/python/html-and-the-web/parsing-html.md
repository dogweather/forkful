---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:45.226842-07:00
description: "Hvordan gj\xF8re det: Python tilbyr kraftfulle biblioteker som BeautifulSoup\
  \ og requests for web scraping og HTML-parsing. For \xE5 begynne, m\xE5 du installere\u2026"
lastmod: '2024-03-13T22:44:40.359035-06:00'
model: gpt-4-0125-preview
summary: Python tilbyr kraftfulle biblioteker som BeautifulSoup og requests for web
  scraping og HTML-parsing.
title: Analysering av HTML
weight: 43
---

## Hvordan gjøre det:
Python tilbyr kraftfulle biblioteker som BeautifulSoup og requests for web scraping og HTML-parsing. For å begynne, må du installere disse bibliotekene hvis du ikke allerede har gjort det:

```bash
pip install beautifulsoup4 requests
```

Her er et grunnleggende eksempel som bruker `requests` for å hente HTML-innholdet til en nettside og `BeautifulSoup` for å parse det:

```python
import requests
from bs4 import BeautifulSoup

# Hent innholdet til en nettside
URL = 'https://example.com'
page = requests.get(URL)

# Parse HTML-innholdet
soup = BeautifulSoup(page.content, 'html.parser')

# Eksempel på uttrekking av nettsidens tittel
title = soup.find('title').text
print(f'Nettsidetittel: {title}')
```

**Eksempel på utdata**:
```
Nettsidetittel: Example Domain
```

For mer komplekse forespørsler, som å trekke ut alle lenker fra en nettside, kan du bruke BeautifulSoup sine ulike metoder for å navigere og søke i parsetreet:

```python
# Trekke ut alle lenker innenfor <a>-tagger
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**Eksempel på utdata**:
```
https://www.iana.org/domains/example
```

BeautifulSoup sin fleksibilitet lar deg tilpasse søket ditt for de nøyaktige dataene som trengs, noe som gjør HTML-parsing til et kraftfullt verktøy for programmere som arbeider med webinnhold.

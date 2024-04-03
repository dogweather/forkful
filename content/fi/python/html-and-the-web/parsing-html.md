---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:55.775292-07:00
description: "Miten: Python tarjoaa tehokkaita kirjastoja, kuten BeautifulSoup ja\
  \ requests, verkkosivujen kaapimiseen ja HTML:n j\xE4sennykseen. Aloittaaksesi sinun\
  \ t\xE4ytyy\u2026"
lastmod: '2024-03-13T22:44:56.141373-06:00'
model: gpt-4-0125-preview
summary: "Python tarjoaa tehokkaita kirjastoja, kuten BeautifulSoup ja requests, verkkosivujen\
  \ kaapimiseen ja HTML:n j\xE4sennykseen."
title: "HTML:n j\xE4sennys"
weight: 43
---

## Miten:
Python tarjoaa tehokkaita kirjastoja, kuten BeautifulSoup ja requests, verkkosivujen kaapimiseen ja HTML:n jäsennykseen. Aloittaaksesi sinun täytyy asentaa nämä kirjastot, jos et ole jo tehnyt niin:

```bash
pip install beautifulsoup4 requests
```

Tässä on perusesimerkki, jossa käytetään `requests`-kirjastoa noutamaan verkkosivun HTML-sisältö ja `BeautifulSoup`-kirjastoa sen jäsennykseen:

```python
import requests
from bs4 import BeautifulSoup

# Nouda verkkosivun sisältö
URL = 'https://example.com'
page = requests.get(URL)

# Jäsennä HTML-sisältö
soup = BeautifulSoup(page.content, 'html.parser')

# Esimerkki verkkosivun otsikon erottamisesta
title = soup.find('title').text
print(f'Verkkosivun otsikko: {title}')
```

**Esimerkkitulo**:
```
Verkkosivun otsikko: Esimerkkialue
```

Monimutkaisempia kyselyitä varten, kuten kaikkien linkkien erottaminen verkkosivulta, voit käyttää BeautifulSoupin eri menetelmiä navigoidaksesi ja etsiäksesi jäsennyspuusta:

```python
# Erottele kaikki linkit <a> tageista
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**Esimerkkitulo**:
```
https://www.iana.org/domains/example
```

BeautifulSoupin joustavuuden ansiosta voit räätälöidä hakuasi tarkalleen tarvitsemallesi datalle, mikä tekee HTML:n jäsennyksestä tehokkaan työkalun ohjelmoijille, jotka työskentelevät web-sisällön parissa.

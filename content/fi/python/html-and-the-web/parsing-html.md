---
title:                "HTML:n jäsennys"
aliases:
- /fi/python/parsing-html.md
date:                  2024-02-03T19:12:55.775292-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
HTML:n jäsennys tarkoittaa verkkosivun HTML-koodin analysointia tiettyjen tietojen tai elementtien erottamiseksi, mikä on yleinen tehtävä verkkosivujen kaapimisessa, datan louhinnassa tai automatisoiduissa vuorovaikutuksissa verkkosivustojen kanssa. Ohjelmoijat tekevät sen ohjelmallisesti vuorovaikuttaakseen verkkosivujen kanssa tai noutaakseen niistä tietoja, automatisoimaan tehtäviä tai testaamaan web-sovelluksia.

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

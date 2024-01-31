---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:33:32.619222-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

HTML:n jäsentäminen tarkoittaa HTML-dokumentin rakenteen lukemista ja sen sisällön muuttamista hyödynnettäväksi muodoksi. Ohjelmoijat jäsentävät HTML:ää esimerkiksi verkkosivujen tietojen kaapimiseen tai sisällön automaattiseen muokkaamiseen.

## Kuinka:

Pythonissa HTML:n jäsentämiseen käytetään usein `BeautifulSoup`-kirjastoa, jota tukee `requests`-kirjasto nettipyyntöjen tekemiseen.

```Python
from bs4 import BeautifulSoup
import requests

# Hae sivun sisältö
sivu = requests.get('https://esimerkki.fi')
sivun_sisalto = sivu.content

# Luo BeautifulSoup-olio
soup = BeautifulSoup(sivun_sisalto, 'html.parser')

# Etsi kaikki 'a'-tagit eli hyperlinkit
linkit = soup.find_all('a')

# Tulosta linkkien teksti ja osoite
for linkki in linkit:
    print(linkki.text, '-', linkki.get('href'))
```

Jos tehdään pyyntö osoitteeseen `https://esimerkki.fi` jossa on linkkejä, saatamme saada tuloksen:

```
Etusivu - /etusivu
Ota Yhteyttä - /yhteys
Tuotteet - /tuotteet
```

## Syväsukellus:

HTML:n jäsentäminen on ollut tärkeää Internetin alkuaikoina lähtien, kun tietojen automaattinen kerääminen ja käsittely on tullut mahdolliseksi. Vaikka `BeautifulSoup` on suosittu valinta, on olemassa muitakin kirjastoja, kuten `lxml` ja `html.parser`, jotka ovat osa Pythonin vakiovarustusta. Kuitenkin, `BeautifulSoup` tarjoaa usein helppokäyttöisemmän ja joustavamman rajapinnan erilaisten HTML-rakenteiden jäsentämiseen.

Tietojen jäsentämisen oikeellisuus ja eettinen puoli on otettava huomioon; eri sivustojen käyttöehdot voivat rajoittaa automatisoitua käsittelyä. On myös muistettava, että HTML-rakenteet voivat muuttua, mikä saattaa tehdä jäsentämiskoodin päivittämisen tarpeelliseksi.

## Katso Myös:

- BeautifulSoup dokumentaatio: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Requests: https://requests.readthedocs.io/en/master/
- W3C HTML5 spesifikaatio: https://www.w3.org/TR/html5/
- Pythonin standardikirjaston `html.parser`: https://docs.python.org/3/library/html.parser.html
- `lxml` parserin dokumentaatio: https://lxml.de/parsing.html

Muista, että Python ja sen kirjastot kehittyvät, tarkista siis aina uusin tieto ja dokumentaatio.

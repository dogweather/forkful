---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/parsing-html.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML-koodin jäsennys tarkoittaa, että analysoimme HTML-merkkijonon ja muutamme sen muotoon, jota ohjelma voi käsitellä (esimerkiksi puurakenteeksi). Ohjelmoijat tekevät tämän, koska se mahdollistaa verkkosivun datan hakemisen ja manipuloinnin.

## Miten:

Käytetään Pythonin kirjastoa `Beautiful Soup` HTML:n jäsennykseen. Asenna se komennolla:

```Python
pip install beautifulsoup4
```

Esimerkkikoodi verkkosivun HTML:n jäsennykseen ja tulostamiseen:

```Python
from bs4 import BeautifulSoup
import requests

sivu = requests.get("https://www.python.org/")

soup = BeautifulSoup(sivu.content, 'html.parser')

print(soup.prettify())
```

## Syvällinen tarkastelu:

HTML:n jäsentäminen on vanha tekniikka, jota on käytetty internetin alkuajoista asti. Vaihtoehtoisia työkaluja ovat esimerkiksi `lxml` ja `html.parser`, mutta `Beautiful Soup` on erittäin suosittu sen helppokäyttöisyyden ansiosta.

`Beautiful Soup` muuntaa HTML:n puurakenteeksi, jolloin voit hakea tietoja tageista ja niiden sisällöstä. Puurakenteen avulla voit esimerkiksi löytää kyseisen tagin vanhemman, sisarukset tai lapset.

## Katso myös:

1. Beautiful Soupin dokumentaatio: https://www.crummy.com/software/BeautifulSoup/docsv
2. W3Schoolsin opas HTML:n jäsentämiseen: https://www.w3schools.com/python/python_parsing.asp
3. Pipy: https://pypi.org/project/beautifulsoup4/
---
title:                "HTML:n jäsentäminen"
html_title:           "Python: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
HTML:n parsiminen on prosessi, jossa tietokone lukee ja analysoi HTML-koodia nähdäkseen, miten verkkosivu on rakennettu. Ohjelmoijat käyttävät tätä tekniikkaa usein, kun he haluavat poimia tietoa verkkosivuilta tai muokata sivuston rakennetta.

## Kuinka tehdä se:
Seuraavassa on esimerkkejä HTML:n parsimiseen käytettävästä koodista ja tulos, joka siitä voidaan saada:

```Python
from bs4 import BeautifulSoup

html = "<html><body><h1>Hello, world!</h1></body></html>"

soup = BeautifulSoup(html, 'html.parser')
print(soup.h1)

# Tulos: <h1>Hello, world!</h1>
```

```Python
from lxml import etree

html = "<html><body><h1>Hello, world!</h1></body></html>"

tree = etree.fromstring(html)
h1 = tree.xpath('//h1/text()')
print(h1)

# Tulos: Hello, world!
```

## Syvempi sukellus:
HTML:n parsiminen alkoi jo 1990-luvulla, kun ensimmäiset web-selaimet kehitettiin. Aiemmin se oli hankalaa ja hidasta, mutta nykyään on kehitetty monia kirjastoja ja työkaluja, kuten BeautifulSoup ja lxml, jotka helpottavat tätä prosessia. On myös muita vaihtoehtoja, kuten käyttöliittymien automaatiotekniikka Selenium, joka voi suorittaa toimintoja selaimessa ja parsia sivuja.

Yksi tärkeä asia, jota kannattaa muistaa HTML:n parsimisessa, on sen oikeaoppinen käyttö. Koska HTML-koodi voi vaihdella sivulta toiselle, on tärkeää varmistaa, että käytetty koodi on yhteensopiva kyseisen sivun kanssa.

Lisäksi monimutkaisten verkkosivujen kanssa voi esiintyä haasteita, koska tiedon poimiminen ja sivun rakenteen muuttaminen voi olla monimutkaista. Siksi on tärkeää ymmärtää HTML:n rakennetta ja käyttää oikeita kirjastoja ja työkaluja.

## Katso myös:
- [BeautifulSoup-dokumentaatio](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [lxml-dokumentaatio](https://lxml.de/)
- [Selenium-dokumentaatio](https://www.selenium.dev/documentation/en/)
- [Regex ja web-skraping](https://hackernoon.com/web-scraping-tutorial-with-python-tips-and-tricks-db070e70e071)
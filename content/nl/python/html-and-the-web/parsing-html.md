---
title:                "HTML Parsen"
aliases:
- /nl/python/parsing-html.md
date:                  2024-01-28T22:04:38.636725-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML parsen is het proces van HTML-code nemen en informatie daaruit extraheren, een beetje zoals het vinden van naalden in een hooiberg – als de hooiberg uit tags bestond en de naalden de gegevens waren die je wilt. Programmeurs doen dit om gegevens van websites te halen, wat alles kan zijn van koppen op een nieuwswebsite tot prijzen in een online winkel.

## Hoe te:

Laten we Python gebruiken om wat gegevens uit een HTML-voorbeeld te grijpen met de `BeautifulSoup` bibliotheek, die het parsen een fluitje van een cent maakt. Installeer het package eerst met `pip install beautifulsoup4`, als je dat nog niet gedaan hebt.

```Python
from bs4 import BeautifulSoup

# Stel je voor dat dit jouw HTML is
html_doc = """
<html>
<head>
    <title>Het verhaal van de Slaapmuis</title>
</head>
<body>
    <p class="title">
        <b>Het verhaal van de Slaapmuis</b>
    </p>
    <p class="story">Er was eens een tijd dat er drie kleine zusjes waren; en hun namen waren
        <a href="http://voorbeeld.com/elsie" class="sister" id="link1">Elsie</a>,
        <a href="http://voorbeeld.com/lacie" class="sister" id="link2">Lacie</a> en
        <a href="http://voorbeeld.com/tillie" class="sister" id="link3">Tillie</a>;
        en ze leefden op de bodem van een put.</p>
</body>
</html>
"""

# Maak het soep
soup = BeautifulSoup(html_doc, 'html.parser')

# Vind de titel-tag
title_tag = soup.title
print("Titel van het verhaal:", title_tag.string)

# Vind alle 'a'-tags met de class 'sister'
sister_tags = soup.find_all('a', class_='sister')
print("Namen en URL's van de zusjes:")
for sister in sister_tags:
    print(f"- Naam: {sister.string}, URL: {sister['href']}")
```

Output zal zijn:

```
Titel van het verhaal: Het verhaal van de Slaapmuis
Namen en URL's van de zusjes:
- Naam: Elsie, URL: http://voorbeeld.com/elsie
- Naam: Lacie, URL: http://voorbeeld.com/lacie
- Naam: Tillie, URL: http://voorbeeld.com/tillie
```

## Diepere Duik

In de vroege dagen van het web, zou je HTML met regex en veel hoop parsen. Dit was rommelig omdat HTML niet altijd netjes en voorspelbaar is. Toen kwamen bibliotheken zoals BeautifulSoup, die door de boomstructuur van HTML navigeren en een zachte manier bieden om de gegevens te snijden en te dobbelen.

Er zijn ook alternatieven zoals `lxml` en `html.parser`, die BeautifulSoup zelf kan gebruiken als parsers. `lxml` is sneller maar minder vergevingsgezind voor slechte HTML, terwijl `html.parser` langzamer is maar niet moeilijk doet over gebroken tags.

Onder de motorkap bouwen deze bibliotheken een parseboom, waardoor tags worden omgezet in objecten waarmee je kunt interageren. BeautifulSoup is als een vriendelijke voorkant voor deze parsers, die jouw vragen vertaalt - zoals "Wat is de titel?" of "Zijn er links hier?" - in acties op de boom.

## Zie Ook

- BeautifulSoup documentatie: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Een introductie tot het parsen van HTML met regex (en waarom je het niet moet doen): https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags
- Web scraping met Python (een praktische gids): https://realpython.com/beautiful-soup-web-scraper-python/

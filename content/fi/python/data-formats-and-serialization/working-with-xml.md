---
date: 2024-01-26 04:34:49.776731-07:00
description: "Kuinka: Pythonin `xml.etree.ElementTree` -moduuli tarjoaa ty\xF6kalut\
  \ XML:n k\xE4sittelyyn. J\xE4senn\xE4 XML-dokumentti."
lastmod: '2024-03-13T22:44:56.167695-06:00'
model: gpt-4-0125-preview
summary: "Pythonin `xml.etree.ElementTree` -moduuli tarjoaa ty\xF6kalut XML:n k\xE4\
  sittelyyn."
title: "XML:n k\xE4sittely"
weight: 40
---

## Kuinka:
Pythonin `xml.etree.ElementTree` -moduuli tarjoaa työkalut XML:n käsittelyyn.

Jäsennä XML-dokumentti:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Learning Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programming Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'Otsikko: {title}, Tekijä: {author}')
```
Esimerkkitulo:
```
Otsikko: Learning Python, Tekijä: Mark Lutz
Otsikko: Programming Python, Tekijä: Mark Lutz
```

Luo XML-dokumentti:
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automate the Boring Stuff with Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## Syväsukellus:
XML on ollut olemassa 90-luvun lopulta lähtien, luotu SGML:n yksinkertaistetuksi alajoukoksi helppoa verkkodatan jakamista varten. Huolimatta JSON:n suosion kasvusta web-datan alueella, XML on edelleen tärkeä monissa enterprise-, konfiguraatio- ja webservices-sovelluksissa (SOAP, RSS).

Vaihtoehtoja `xml.etree.ElementTree`:lle ovat `lxml` ja `minidom`. `lxml` on nopeampi ja ominaisuuksiltaan rikkaampi, kun taas `minidom` tarjoaa enemmän "DOM-tyylisen" XML-rajapinnan. Valinnoissa kannattaa harkita käytön helppoutta, suorituskykyä ja erityisiä ominaisuusvaatimuksia.

Sisäisesti `ElementTree` toimii elementtipuumallilla, jossa XML-tiedoston jokainen osa on solmu puussa. Tämä mahdollistaa suoraviivaiset polkulausekkeet ja haut, mikä tekee XML-datan rakenteen navigoinnista ja manipuloinnista helpompaa.

## Katso myös:
- Python `xml.etree.ElementTree` -moduuli: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML-opas: https://www.w3schools.com/xml/
- XML-spesifikaatio: https://www.w3.org/XML/

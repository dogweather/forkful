---
date: 2024-01-26 04:34:53.948477-07:00
description: "\"Arbeid med XML\" refererer til prosessen med \xE5 lese, opprette og\
  \ modifisere XML (eXtensible Markup Language) filer ved hjelp av programmering.\u2026"
lastmod: '2024-03-13T22:44:40.385178-06:00'
model: gpt-4-0125-preview
summary: "\"Arbeid med XML\" refererer til prosessen med \xE5 lese, opprette og modifisere\
  \ XML (eXtensible Markup Language) filer ved hjelp av programmering.\u2026"
title: "\xC5 jobbe med XML"
weight: 40
---

## Hva & Hvorfor?
"Arbeid med XML" refererer til prosessen med å lese, opprette og modifisere XML (eXtensible Markup Language) filer ved hjelp av programmering. Programmerere gjør dette fordi XML er mye brukt til datautveksling på grunn av sin plattformuavhengige natur og selvbeskrivende format.

## Hvordan:
Pythons `xml.etree.ElementTree`-modul tilbyr verktøy for å arbeide med XML.

Parse et XML-dokument:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Lær Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programmer Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'Tittel: {title}, Forfatter: {author}')
```
Eksempel på utdata:
```
Tittel: Lær Python, Forfatter: Mark Lutz
Tittel: Programmer Python, Forfatter: Mark Lutz
```

Opprett et XML-dokument:
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automatiser det kjedelige med Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## Dypdykk:
XML har vært rundt siden slutten av 90-tallet, opprettet som en forenklet delmengde av SGML for enkel online datadeling. Til tross for JSONs økende popularitet for webdata, forblir XML viktig i mange bedrifts, konfigurasjons- og webtjenester (SOAP, RSS).

Alternativer til `xml.etree.ElementTree` inkluderer `lxml` og `minidom`. `lxml` er raskere og har flere funksjoner, mens `minidom` tilbyr et mer "DOM-lignende" XML-grensesnitt. Når du velger, vurder brukervennlighet, ytelse og spesifikke funksjonskrav.

Under panseret opererer `ElementTree` på en elementtremodell, hvor hver komponent av XML-filen er en node i et tre. Dette muliggjør enkle stiuttrykk og søk, noe som gjør det lettere å navigere og manipulere strukturen til XML-data.

## Se også:
- Python `xml.etree.ElementTree`-modul: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML-opplæring: https://www.w3schools.com/xml/
- XML-spesifikasjon: https://www.w3.org/XML/

---
title:                "Att arbeta med XML"
aliases:
- /sv/python/working-with-xml/
date:                  2024-01-26T04:35:00.636311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
"Arbeta med XML" avser processen att läsa, skapa och modifiera XML (eXtensible Markup Language)-filer med programmering. Programmerare gör det eftersom XML är brett använd för datautbyte på grund av dess plattformsoberoende natur och självbeskrivande format.

## Hur man gör:
Pythons modul `xml.etree.ElementTree` erbjuder verktyg för att arbeta med XML.

Analysera ett XML-dokument:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Lära sig Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programmera med Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'Titel: {title}, Författare: {author}')
```
Exempel på utdata:
```
Titel: Lära sig Python, Författare: Mark Lutz
Titel: Programmera med Python, Författare: Mark Lutz
```

Skapa ett XML-dokument:
```python
bibliotek = ET.Element('library')
bok = ET.SubElement(bibliotek, 'book')
titel = ET.SubElement(bok, 'title')
titel.text = 'Automatisera det tråkiga med Python'
författare = ET.SubElement(bok, 'author')
författare.text = 'Al Sweigart'

träd = ET.ElementTree(bibliotek)
träd.write('library.xml')
```

## Fördjupning:
XML har funnits sedan slutet av 90-talet, skapat som en förenklad delmängd av SGML för enkel online-datadelning. Trots JSON:s ökande popularitet för webbdata förblir XML vitalt i många företag, konfigurationer och webbtjänster (SOAP, RSS).

Alternativ till `xml.etree.ElementTree` innefattar `lxml` och `minidom`. `lxml` är snabbare och har fler funktioner, medan `minidom` erbjuder ett mer "DOM-liknande" XML-gränssnitt. När du väljer, överväg användarvänlighet, prestanda och specifika funktionskrav.

Under ytan opererar `ElementTree` på en elementträdsmodell, där varje komponent i XML-filen är en nod i ett träd. Detta möjliggör enkel användning av sökvägar och sökningar, vilket gör det lättare att navigera och manipulera strukturen av XML-data.

## Se även:
- Python `xml.etree.ElementTree`-modul: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML-tutorial: https://www.w3schools.com/xml/
- XML-specifikation: https://www.w3.org/XML/

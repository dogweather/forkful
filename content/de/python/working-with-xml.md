---
title:                "Arbeiten mit XML"
date:                  2024-01-26T04:34:31.753434-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"

category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
"Arbeiten mit XML" bezieht sich auf den Prozess des Lesens, Erstellens und Modifizierens von XML (eXtensible Markup Language)-Dateien mittels Programmierung. Programmierer tun dies, weil XML aufgrund seiner plattformunabhängigen Natur und seines selbstbeschreibenden Formats weit verbreitet für den Datenaustausch verwendet wird.

## Wie geht das:
Das Modul `xml.etree.ElementTree` von Python bietet Werkzeuge zur Arbeit mit XML.

Ein XML-Dokument parsen:
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
    print(f'Titel: {title}, Autor: {author}')
```
Beispielausgabe:
```
Titel: Learning Python, Autor: Mark Lutz
Titel: Programming Python, Autor: Mark Lutz
```

Ein XML-Dokument erstellen:
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

## Tiefergehender Einblick:
XML existiert seit den späten 90ern, geschaffen als eine vereinfachte Untergruppe von SGML für den einfachen Online-Datenaustausch. Trotz der steigenden Beliebtheit von JSON für Webdaten bleibt XML in vielen Unternehmen, Konfigurationen und Webservices (SOAP, RSS) unverzichtbar.

Alternativen zu `xml.etree.ElementTree` umfassen `lxml` und `minidom`. `lxml` ist schneller und funktionsreicher, wohingegen `minidom` eine "DOM-ähnlichere" XML-Schnittstelle bietet. Bei der Auswahl sollte man Benutzerfreundlichkeit, Leistung und spezifische Funktionsanforderungen berücksichtigen.

Unter der Haube operiert `ElementTree` auf einem Elementbaummodell, in dem jede Komponente der XML-Datei ein Knoten in einem Baum ist. Dies ermöglicht einfache Pfadausdrücke und Suchvorgänge, was es leichter macht, die Struktur von XML-Daten zu navigieren und zu manipulieren.

## Siehe auch:
- Python `xml.etree.ElementTree`-Modul: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML-Tutorial: https://www.w3schools.com/xml/
- XML-Spezifikation: https://www.w3.org/XML/

---
date: 2024-01-26 04:34:59.617139-07:00
description: "\"Praca z XML\" odnosi si\u0119 do procesu odczytu, tworzenia i modyfikacji\
  \ plik\xF3w XML (eXtensible Markup Language) za pomoc\u0105 programowania. Programi\u015B\
  ci robi\u0105\u2026"
lastmod: '2024-02-25T18:49:33.400451-07:00'
model: gpt-4-0125-preview
summary: "\"Praca z XML\" odnosi si\u0119 do procesu odczytu, tworzenia i modyfikacji\
  \ plik\xF3w XML (eXtensible Markup Language) za pomoc\u0105 programowania. Programi\u015B\
  ci robi\u0105\u2026"
title: Praca z XML
---

{{< edit_this_page >}}

## Co i dlaczego?
"Praca z XML" odnosi się do procesu odczytu, tworzenia i modyfikacji plików XML (eXtensible Markup Language) za pomocą programowania. Programiści robią to, ponieważ XML jest szeroko stosowany do wymiany danych ze względu na jego niezależność od platformy oraz samoopisowy format.

## Jak to zrobić:
Moduł `xml.etree.ElementTree` w Pythonie oferuje narzędzia do pracy z XML.

Przetwarzanie dokumentu XML:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Nauka Pythona</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programowanie w Pythonie</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'Tytuł: {title}, Autor: {author}')
```
Przykładowe wyjście:
```
Tytuł: Nauka Pythona, Autor: Mark Lutz
Tytuł: Programowanie w Pythonie, Autor: Mark Lutz
```

Tworzenie dokumentu XML:
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automatyzowanie nudnych zadań z Pythonem'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## Pogłębiona analiza:
XML istnieje od końca lat '90, stworzony jako uproszczony podzbiór SGML dla łatwego internetowego udostępniania danych. Pomimo rosnącej popularności JSON dla danych internetowych, XML pozostaje kluczowy w wielu przedsiębiorstwach, konfiguracjach oraz usługach internetowych (SOAP, RSS).

Alternatywy dla `xml.etree.ElementTree` obejmują `lxml` i `minidom`. `lxml` jest szybszy i bogatszy w funkcje, podczas gdy `minidom` zapewnia bardziej "DOM-podobny" interfejs XML. Przy wyborze należy wziąć pod uwagę łatwość użycia, wydajność i specyficzne wymagania funkcjonalne.

Wewnątrz, `ElementTree` działa na modelu drzewa elementów, gdzie każdy komponent pliku XML jest węzłem w drzewie. Pozwala to na proste wyrażenia ścieżek i wyszukiwania, ułatwiając nawigację i manipulację strukturą danych XML.

## Zobacz również:
- Moduł Pythona `xml.etree.ElementTree`: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- Tutorial XML z W3Schools: https://www.w3schools.com/xml/
- Specyfikacja XML: https://www.w3.org/XML/

---
date: 2024-01-25 03:39:34.032559-07:00
description: "\"Working with XML\" refers to the process of reading, creating, and\
  \ modifying XML (eXtensible Markup Language) files using programming. Programmers\
  \ do it\u2026"
lastmod: '2024-03-11T00:14:33.579893-06:00'
model: gpt-4-1106-preview
summary: "\"Working with XML\" refers to the process of reading, creating, and modifying\
  \ XML (eXtensible Markup Language) files using programming. Programmers do it\u2026"
title: Working with XML
---

{{< edit_this_page >}}

## What & Why?
"Working with XML" refers to the process of reading, creating, and modifying XML (eXtensible Markup Language) files using programming. Programmers do it because XML is widely used for data exchange due to its platform-independent nature and self-describing format.

## How to:
Python's `xml.etree.ElementTree` module offers tools to work with XML. 

Parse an XML document:
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
    print(f'Title: {title}, Author: {author}')
```
Sample output:
```
Title: Learning Python, Author: Mark Lutz
Title: Programming Python, Author: Mark Lutz
```

Create an XML document:
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

## Deep Dive:
XML has been around since the late '90s, created as a simplified subset of SGML for easy online data sharing. Despite JSON's rising popularity for web data, XML remains vital in many enterprise, configuration, and web services (SOAP, RSS).

Alternatives to `xml.etree.ElementTree` include `lxml` and `minidom`. `lxml` is faster and more feature-rich, whereas `minidom` provides a more "DOM-like" XML interface. When choosing, consider ease of use, performance, and specific feature requirements.

Under the hood, `ElementTree` operates on an element tree model, where each component of the XML file is a node in a tree. This allows for straightforward path expressions and searches, making it easier to navigate and manipulate the structure of XML data.

## See Also:
- Python `xml.etree.ElementTree` module: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML tutorial: https://www.w3schools.com/xml/
- XML Specification: https://www.w3.org/XML/

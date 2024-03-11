---
date: 2024-01-26 04:34:29.135038-07:00
description: "\"Trabajar con XML\" se refiere al proceso de leer, crear y modificar\
  \ archivos XML (eXtensible Markup Language) utilizando programaci\xF3n. Los programadores\u2026"
lastmod: '2024-03-11T00:14:32.468512-06:00'
model: gpt-4-0125-preview
summary: "\"Trabajar con XML\" se refiere al proceso de leer, crear y modificar archivos\
  \ XML (eXtensible Markup Language) utilizando programaci\xF3n. Los programadores\u2026"
title: Trabajando con XML
---

{{< edit_this_page >}}

## Qué y Por Qué?
"Trabajar con XML" se refiere al proceso de leer, crear y modificar archivos XML (eXtensible Markup Language) utilizando programación. Los programadores lo hacen porque el XML se usa ampliamente para el intercambio de datos debido a su naturaleza independiente de la plataforma y formato auto-descriptivo.

## Cómo hacerlo:
El módulo `xml.etree.ElementTree` de Python ofrece herramientas para trabajar con XML.

Analizar un documento XML:
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
    print(f'Título: {title}, Autor: {author}')
```
Ejemplo de salida:
```
Título: Learning Python, Autor: Mark Lutz
Título: Programming Python, Autor: Mark Lutz
```

Crear un documento XML:
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

## Estudio Profundo:
El XML existe desde finales de los '90, creado como un subconjunto simplificado de SGML para compartir datos en línea fácilmente. A pesar de la creciente popularidad de JSON para datos web, el XML sigue siendo vital en muchas empresas, configuraciones y servicios web (SOAP, RSS).

Las alternativas a `xml.etree.ElementTree` incluyen `lxml` y `minidom`. `lxml` es más rápido y rico en características, mientras que `minidom` proporciona una interfaz XML más "similar a DOM". Al elegir, considere la facilidad de uso, rendimiento y requisitos de características específicas.

Bajo el capó, `ElementTree` opera en un modelo de árbol de elementos, donde cada componente del archivo XML es un nodo en un árbol. Esto permite expresiones de ruta sencillas y búsquedas, facilitando la navegación y manipulación de la estructura de datos XML.

## Ver También:
- Módulo Python `xml.etree.ElementTree`: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- Tutorial de XML de W3Schools: https://www.w3schools.com/xml/
- Especificación de XML: https://www.w3.org/XML/

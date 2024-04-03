---
date: 2024-01-26 04:34:52.005042-07:00
description: "Como fazer: O m\xF3dulo `xml.etree.ElementTree` do Python oferece ferramentas\
  \ para trabalhar com XML. Analisar um documento XML."
lastmod: '2024-03-13T22:44:46.179613-06:00'
model: gpt-4-0125-preview
summary: "O m\xF3dulo `xml.etree.ElementTree` do Python oferece ferramentas para trabalhar\
  \ com XML."
title: Trabalhando com XML
weight: 40
---

## Como fazer:
O módulo `xml.etree.ElementTree` do Python oferece ferramentas para trabalhar com XML.

Analisar um documento XML:
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
Saída de exemplo:
```
Título: Learning Python, Autor: Mark Lutz
Título: Programming Python, Autor: Mark Lutz
```

Criar um documento XML:
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

## Aprofundamento:
O XML existe desde o final dos anos 90, criado como um subconjunto simplificado do SGML para facilitar o compartilhamento de dados online. Apesar da crescente popularidade do JSON para dados web, o XML continua vital em muitas empresas, configurações e serviços web (SOAP, RSS).

Alternativas ao `xml.etree.ElementTree` incluem `lxml` e `minidom`. O `lxml` é mais rápido e possui mais recursos, enquanto o `minidom` oferece uma interface XML mais "DOM-like". Ao escolher, considere a facilidade de uso, desempenho e requisitos específicos de recursos.

Por trás dos panos, o `ElementTree` opera em um modelo de árvore de elementos, onde cada componente do arquivo XML é um nó em uma árvore. Isso permite expressões de caminho e buscas diretas, facilitando a navegação e manipulação da estrutura de dados XML.

## Veja Também:
- Módulo `xml.etree.ElementTree` do Python: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- Tutorial de XML da W3Schools: https://www.w3schools.com/xml/
- Especificação XML: https://www.w3.org/XML/

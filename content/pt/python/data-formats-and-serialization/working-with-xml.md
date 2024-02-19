---
aliases:
- /pt/python/working-with-xml/
date: 2024-01-26 04:34:52.005042-07:00
description: "\"Trabalhando com XML\" refere-se ao processo de leitura, cria\xE7\xE3\
  o e modifica\xE7\xE3o de arquivos XML (eXtensible Markup Language) utilizando programa\xE7\
  \xE3o. Os\u2026"
lastmod: 2024-02-18 23:08:57.785295
model: gpt-4-0125-preview
summary: "\"Trabalhando com XML\" refere-se ao processo de leitura, cria\xE7\xE3o\
  \ e modifica\xE7\xE3o de arquivos XML (eXtensible Markup Language) utilizando programa\xE7\
  \xE3o. Os\u2026"
title: Trabalhando com XML
---

{{< edit_this_page >}}

## O Que & Porquê?
"Trabalhando com XML" refere-se ao processo de leitura, criação e modificação de arquivos XML (eXtensible Markup Language) utilizando programação. Os programadores fazem isso porque o XML é amplamente utilizado para troca de dados devido à sua natureza independente de plataforma e formato autoexplicativo.

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

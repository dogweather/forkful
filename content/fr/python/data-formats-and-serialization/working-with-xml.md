---
date: 2024-01-26 04:35:08.721524-07:00
description: 'Comment faire : Le module `xml.etree.ElementTree` de Python offre des
  outils pour travailler avec XML. Analyser un document XML .'
lastmod: '2024-03-13T22:44:57.262928-06:00'
model: gpt-4-0125-preview
summary: Le module `xml.etree.ElementTree` de Python offre des outils pour travailler
  avec XML.
title: Travailler avec XML
weight: 40
---

## Comment faire :
Le module `xml.etree.ElementTree` de Python offre des outils pour travailler avec XML.

Analyser un document XML :
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
    print(f'Titre: {title}, Auteur: {author}')
```
Sortie d'exemple :
```
Titre: Learning Python, Auteur: Mark Lutz
Titre: Programming Python, Auteur: Mark Lutz
```

Créer un document XML :
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

## Approfondissement :
XML existe depuis la fin des années 90, créé comme un sous-ensemble simplifié de SGML pour faciliter le partage de données en ligne. Malgré la popularité croissante de JSON pour les données web, XML reste vital dans de nombreux domaines d'entreprise, de configuration et de services web (SOAP, RSS).

Les alternatives à `xml.etree.ElementTree` incluent `lxml` et `minidom`. `lxml` est plus rapide et plus riche en fonctionnalités, tandis que `minidom` offre une interface XML plus "DOM-like". Lors du choix, considérez la facilité d'utilisation, la performance, et les besoins spécifiques en fonctionnalités.

Sous le capot, `ElementTree` fonctionne sur un modèle d'arbre d'éléments, où chaque composant du fichier XML est un nœud dans un arbre. Ceci permet des expressions de chemin et des recherches simples, rendant plus facile la navigation et la manipulation de la structure des données XML.

## Voir aussi :
- Module Python `xml.etree.ElementTree` : https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml` : https://lxml.de/
- Tutoriel XML de W3Schools : https://www.w3schools.com/xml/
- Spécification XML : https://www.w3.org/XML/

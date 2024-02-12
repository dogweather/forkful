---
title:                "Travailler avec XML"
aliases:
- /fr/python/working-with-xml/
date:                  2024-01-26T04:35:08.721524-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
"Travailler avec XML" se réfère au processus de lecture, création et modification de fichiers XML (eXtensible Markup Language) en utilisant la programmation. Les programmeurs le font parce que XML est largement utilisé pour l'échange de données en raison de sa nature indépendante de la plateforme et de son format auto-descriptif.

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

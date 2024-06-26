---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:29.496116-07:00
description: 'Hoe: Pythons `xml.etree.ElementTree` module biedt hulpmiddelen om met
  XML te werken. Een XML-document parseren.'
lastmod: '2024-03-13T22:44:50.400448-06:00'
model: gpt-4-0125-preview
summary: Pythons `xml.etree.ElementTree` module biedt hulpmiddelen om met XML te werken.
title: Werken met XML
weight: 40
---

## Hoe:
Pythons `xml.etree.ElementTree` module biedt hulpmiddelen om met XML te werken.

Een XML-document parseren:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<bibliotheek>
    <boek>
        <titel>Leer Python</titel>
        <auteur>Mark Lutz</auteur>
    </boek>
    <boek>
        <titel>Python Programmeren</titel>
        <auteur>Mark Lutz</auteur>
    </boek>
</bibliotheek>
"""

root = ET.fromstring(xml_data)
for boek in root.findall('boek'):
    titel = boek.find('titel').text
    auteur = boek.find('auteur').text
    print(f'Titel: {titel}, Auteur: {auteur}')
```
Voorbeelduitvoer:
```
Titel: Leer Python, Auteur: Mark Lutz
Titel: Python Programmeren, Auteur: Mark Lutz
```

Een XML-document creëren:
```python
bibliotheek = ET.Element('bibliotheek')
boek = ET.SubElement(bibliotheek, 'boek')
titel = ET.SubElement(boek, 'titel')
titel.text = 'Automatiseer het Vervelende Werk met Python'
auteur = ET.SubElement(boek, 'auteur')
auteur.text = 'Al Sweigart'

tree = ET.ElementTree(bibliotheek)
tree.write('bibliotheek.xml')
```

## Diepgaand:
XML bestaat al sinds de late jaren '90, gecreëerd als een vereenvoudigde subset van SGML voor gemakkelijke online gegevensdeling. Ondanks de toenemende populariteit van JSON voor webgegevens, blijft XML essentieel in vele ondernemingen, configuraties en webservices (SOAP, RSS).

Alternatieven voor `xml.etree.ElementTree` zijn onder andere `lxml` en `minidom`. `lxml` is sneller en heeft meer functies, terwijl `minidom` een meer "DOM-achtige" XML-interface biedt. Bij het kiezen moet men rekening houden met gebruiksgemak, prestaties en specifieke kenmerkvereisten.

Onder de motorkap werkt `ElementTree` op een elementen boommodel, waarbij elk onderdeel van het XML-bestand een knooppunt in een boom is. Dit maakt eenvoudige paduitdrukkingen en zoekopdrachten mogelijk, waardoor het gemakkelijker is om de structuur van XML-gegevens te navigeren en manipuleren.

## Zie ook:
- Python `xml.etree.ElementTree` module: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML-tutorial: https://www.w3schools.com/xml/
- XML-specificatie: https://www.w3.org/XML/

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:31.885458-07:00
description: "Werken met XML betekent het parseren en genereren van XML-gegevens in\
  \ Swift. Programmeurs doen dit voor gegevensuitwisseling, vooral wanneer ze integreren\u2026"
lastmod: '2024-03-13T22:44:51.182219-06:00'
model: gpt-4-0125-preview
summary: Werken met XML betekent het parseren en genereren van XML-gegevens in Swift.
title: Werken met XML
weight: 40
---

## Wat & Waarom?
Werken met XML betekent het parseren en genereren van XML-gegevens in Swift. Programmeurs doen dit voor gegevensuitwisseling, vooral wanneer ze integreren met systemen waar XML het standaardformaat is.

## Hoe:
Swift biedt `XMLParser` en `XMLDocument` voor het parseren van XML-gegevens. Hier is een fragment om een eenvoudige XML-string te parseren:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Herinnering</heading>
    <body>Vergeet het feestje op vrijdag niet!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Jouw XMLParserDelegate
    parser.parse()
}
```

Je kunt ook XML genereren met `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Voorbeelduitvoer:

```xml
<note>
  <to>Tove</to>
</note>
```

## Diepgaande Duik
XML, ofwel Extensible Markup Language, bestaat al sinds de late jaren '90. Het is omslachtig maar leesbaar voor mensen, wat het geschikt maakt voor complexe gegevensstructuren. Swifts XML-parseermogelijkheden zijn niet zo robuust als die gevonden in Python's ElementTree of Java's JAXB, maar ze volstaan voor de basisbehoeften.

Alternatieven zoals JSON worden vaak verkozen in nieuwe systemen vanwege hun lichtere gewicht en minder complexe parsers, maar XML is nog steeds prominent aanwezig in veel bedrijfs- en legacy-systemen.

Wanneer je met XML in Swift werkt, is `XMLParser` een stream-gebaseerde parser, wat betekent dat het sequentieel door het XML-document leest. Voor grote XML-bestanden is dit geheugen-efficiënt. Echter, als je op zoek bent naar eenvoud en je XML-gegevens redelijk klein zijn, dan kan het gebruik van `XMLDocument` eenvoudiger zijn.

## Zie Ook
- [Apple's XML Parsing Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/)

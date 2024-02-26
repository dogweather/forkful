---
date: 2024-01-26 04:35:52.209271-07:00
description: "Travailler avec XML signifie analyser et g\xE9n\xE9rer des donn\xE9\
  es XML en Swift. Les programmeurs font cela pour l'\xE9change de donn\xE9es, surtout\
  \ lorsqu'ils\u2026"
lastmod: '2024-02-25T18:49:54.890415-07:00'
model: gpt-4-0125-preview
summary: "Travailler avec XML signifie analyser et g\xE9n\xE9rer des donn\xE9es XML\
  \ en Swift. Les programmeurs font cela pour l'\xE9change de donn\xE9es, surtout\
  \ lorsqu'ils\u2026"
title: Travailler avec XML
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Travailler avec XML signifie analyser et générer des données XML en Swift. Les programmeurs font cela pour l'échange de données, surtout lorsqu'ils s'intègrent à des systèmes où XML est le format standard.

## Comment faire :
Swift fournit `XMLParser` et `XMLDocument` pour analyser les données XML. Voici un extrait pour analyser une chaîne XML simple :

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Rappel</heading>
    <body>N'oubliez pas la fête vendredi !</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Votre XMLParserDelegate
    parser.parse()
}
```

Vous pouvez également générer du XML en utilisant `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Sortie d'exemple :

```xml
<note>
  <to>Tove</to>
</note>
```

## Approfondissement
XML, ou Extensible Markup Language, existe depuis la fin des années 90. Il est verbeux mais lisible par l'homme, ce qui le rend adapté aux structures de données complexes. Les capacités d'analyse XML de Swift ne sont pas aussi robustes que celles trouvées dans ElementTree de Python ou JAXB de Java, mais elles sont suffisantes pour les besoins de base.

Des alternatives comme JSON sont souvent préférées dans les nouveaux systèmes en raison de leur légèreté et de leurs parseurs moins complexes, mais XML reste prédominant dans de nombreux systèmes d'entreprise et systèmes hérités.

Lors du travail avec XML en Swift, `XMLParser` est un analyseur basé sur des flux, ce qui signifie qu'il lit le document XML séquentiellement. Pour les grands fichiers XML, ceci est efficace en termes de mémoire. Cependant, si vous recherchez la simplicité et que vos données XML sont raisonnablement petites, utiliser `XMLDocument` pourrait être plus direct.

## Voir également
- [Guide de l'analyse XML d'Apple](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [Tutoriel XML de W3Schools](https://www.w3schools.com/xml/)

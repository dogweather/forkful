---
date: 2024-01-26 04:35:55.636879-07:00
description: "Mit XML arbeiten bedeutet, XML-Daten in Swift zu parsen und zu generieren.\
  \ Programmierer tun dies f\xFCr den Datenaustausch, insbesondere wenn sie sich in\u2026"
lastmod: '2024-03-13T22:44:54.248831-06:00'
model: gpt-4-0125-preview
summary: Mit XML arbeiten bedeutet, XML-Daten in Swift zu parsen und zu generieren.
title: Arbeiten mit XML
weight: 40
---

## Wie man:
Swift bietet `XMLParser` und `XMLDocument` zum Parsen von XML-Daten. Hier ist ein Snippet, um einen einfachen XML-String zu parsen:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Vergiss nicht die Party am Freitag!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Ihr XMLParserDelegate
    parser.parse()
}
```

Sie können auch mit `XMLDocument` XML erzeugen:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Beispielausgabe:

```xml
<note>
  <to>Tove</to>
</note>
```

## Tiefergehende Betrachtung
XML, oder Extensible Markup Language, gibt es seit den späten 90ern. Es ist umständlich, aber menschenlesbar, was es für komplexe Datenstrukturen geeignet macht. Swifts Fähigkeiten, XML zu parsen, sind nicht so robust wie die in Pythons ElementTree oder Javas JAXB gefundenen, aber sie sind ausreichend für grundlegende Bedürfnisse.

Alternativen wie JSON werden in neuen Systemen oft bevorzugt aufgrund ihres geringeren Gewichts und der weniger komplexen Parser, aber XML ist immer noch prominent in vielen Unternehmens- und Legacy-Systemen.

Beim Arbeiten mit XML in Swift ist `XMLParser` ein streambasierter Parser, was bedeutet, dass er das XML-Dokument sequenziell liest. Für große XML-Dateien ist dies speichereffizient. Wenn Sie jedoch Einfachheit suchen und Ihre XML-Daten vernünftigerweise klein sind, könnte die Verwendung von `XMLDocument` direkter sein.

## Siehe Auch
- [Apples XML Parsing Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/)

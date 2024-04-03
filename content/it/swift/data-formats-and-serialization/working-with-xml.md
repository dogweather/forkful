---
date: 2024-01-26 04:35:56.707211-07:00
description: "Lavorare con XML significa analizzare e generare dati XML in Swift.\
  \ I programmatori lo fanno per lo scambio di dati, specialmente quando si integrano\
  \ con\u2026"
lastmod: '2024-03-13T22:44:43.794270-06:00'
model: gpt-4-0125-preview
summary: Lavorare con XML significa analizzare e generare dati XML in Swift.
title: Lavorare con XML
weight: 40
---

## Cosa & Perché?
Lavorare con XML significa analizzare e generare dati XML in Swift. I programmatori lo fanno per lo scambio di dati, specialmente quando si integrano con sistemi dove l'XML è il formato standard.

## Come fare:
Swift fornisce `XMLParser` e `XMLDocument` per l'analisi dei dati XML. Ecco un frammento per analizzare una semplice stringa XML:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Promemoria</heading>
    <body>Non dimenticare la festa di venerdì!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Il tuo XMLParserDelegate
    parser.parse()
}
```

È possibile anche generare XML usando `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Output di esempio:

```xml
<note>
  <to>Tove</to>
</note>
```

## Approfondimento
XML, o eXtensible Markup Language, è presente sin dalla fine degli anni '90. È prolisso ma leggibile dall'uomo, rendendolo adatto per strutture dati complesse. Le capacità di analisi XML di Swift non sono robuste come quelle trovate in ElementTree di Python o JAXB di Java, ma sono sufficienti per le necessità di base.

Alternative come JSON sono spesso preferite nei nuovi sistemi dovuto al loro minor peso e parser meno complessi, ma XML è ancora prominente in molti sistemi aziendali e legacy.

Quando si lavora con XML in Swift, `XMLParser` è un parser basato su stream, il che significa che legge sequenzialmente il documento XML. Per file XML di grandi dimensioni, questo è efficiente in termini di memoria. Tuttavia, se cerchi semplicità e i tuoi dati XML sono ragionevolmente piccoli, usare `XMLDocument` potrebbe essere più diretto.

## Vedi Anche
- [Guida all'Analisi XML di Apple](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [Tutorial XML di W3Schools](https://www.w3schools.com/xml/)

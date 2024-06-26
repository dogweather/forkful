---
date: 2024-01-26 04:36:13.459688-07:00
description: "Hvordan: Swift gir `XMLParser` og `XMLDocument` for \xE5 analysere XML-data.\
  \ Her er et eksempel for \xE5 analysere en enkel XML-streng."
lastmod: '2024-03-13T22:44:41.166348-06:00'
model: gpt-4-0125-preview
summary: "Swift gir `XMLParser` og `XMLDocument` for \xE5 analysere XML-data."
title: "\xC5 jobbe med XML"
weight: 40
---

## Hvordan:
Swift gir `XMLParser` og `XMLDocument` for å analysere XML-data. Her er et eksempel for å analysere en enkel XML-streng:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Påminnelse</heading>
    <body>Ikke glem festen på fredag!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Din XMLParserDelegate
    parser.parse()
}
```

Du kan også generere XML ved hjelp av `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Eksempel på utdata:

```xml
<note>
  <to>Tove</to>
</note>
```

## Dypdykk
XML, eller Extensible Markup Language, har vært rundt siden slutten av 90-tallet. Det er ordrikt men menneskelesbart, noe som gjør det godt egnet for komplekse datastrukturer. Swifts XML-analysekapasiteter er ikke like robuste som de man finner i Pythons ElementTree eller Javas JAXB, men de utfører jobben for grunnleggende behov.

Alternativer som JSON foretrekkes ofte i nye systemer på grunn av deres lettere vekt og mindre komplekse parsere, men XML er fortsatt fremtredende i mange foretaks- og arvesystemer.

Når du jobber med XML i Swift, er `XMLParser` en strømbasert parser, noe som betyr at den leser gjennom XML-dokumentet sekvensielt. For store XML-filer er dette minneeffektivt. Imidlertid, hvis du ser etter enkelhet og XML-dataene dine er rimelig små, kan det å bruke `XMLDocument` være mer greit.

## Se også
- [Apples veiledning til XML-parsing](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/)

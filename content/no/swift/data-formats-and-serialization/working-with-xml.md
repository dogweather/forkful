---
aliases:
- /no/swift/working-with-xml/
date: 2024-01-26 04:36:13.459688-07:00
description: "\xC5 jobbe med XML betyr \xE5 analysere og generere XML-data i Swift.\
  \ Programmerere gj\xF8r dette for datamellomlagring, spesielt n\xE5r de integrerer\
  \ med systemer\u2026"
lastmod: 2024-02-18 23:08:54.292426
model: gpt-4-0125-preview
summary: "\xC5 jobbe med XML betyr \xE5 analysere og generere XML-data i Swift. Programmerere\
  \ gj\xF8r dette for datamellomlagring, spesielt n\xE5r de integrerer med systemer\u2026"
title: "\xC5 jobbe med XML"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML betyr å analysere og generere XML-data i Swift. Programmerere gjør dette for datamellomlagring, spesielt når de integrerer med systemer der XML er standardformatet.

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

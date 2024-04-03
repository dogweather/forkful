---
date: 2024-01-26 04:36:05.757421-07:00
description: "Hur man g\xF6r: Swift tillhandah\xE5ller `XMLParser` och `XMLDocument`\
  \ f\xF6r att tolka XML-data. H\xE4r \xE4r ett kodsnutt f\xF6r att tolka en enkel\
  \ XML-str\xE4ng."
lastmod: '2024-03-13T22:44:38.275658-06:00'
model: gpt-4-0125-preview
summary: "Swift tillhandah\xE5ller `XMLParser` och `XMLDocument` f\xF6r att tolka\
  \ XML-data."
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
Swift tillhandahåller `XMLParser` och `XMLDocument` för att tolka XML-data. Här är ett kodsnutt för att tolka en enkel XML-sträng:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Påminnelse</heading>
    <body>Glöm inte bort festen på fredag!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Din XMLParserDelegate
    parser.parse()
}
```

Du kan också generera XML med hjälp av `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Exempel på utdata:

```xml
<note>
  <to>Tove</to>
</note>
```

## Fördjupning
XML, eller Extensible Markup Language, har funnits sedan slutet av 90-talet. Det är utförligt men läsbart för människor, vilket gör det lämpligt för komplexa datastrukturer. Swifts förmåga att tolka XML är inte lika robust som de som finns i Pythons ElementTree eller Javas JAXB, men de klarar sig för grundläggande behov.

Alternativ som JSON föredras ofta i nya system på grund av deras lägre vikt och mindre komplexa tolkare, men XML är fortfarande framträdande i många företag och äldre system.

När man arbetar med XML i Swift, är `XMLParser` en ström-baserad tolkare vilket betyder att den läser igenom XML-dokumentet sekventiellt. För stora XML-filer är detta minneseffektivt. Men om du letar efter enkelhet och din XML-data är rimligen liten, kan användning av `XMLDocument` vara mer rakt på sak.

## Se även
- [Apples guide till XML-tolkning](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML-tutorial](https://www.w3schools.com/xml/)

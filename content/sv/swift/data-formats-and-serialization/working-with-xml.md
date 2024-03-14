---
date: 2024-01-26 04:36:05.757421-07:00
description: "Att arbeta med XML inneb\xE4r att tolka och generera XML-data i Swift.\
  \ Programmerare g\xF6r detta f\xF6r datautbyte, s\xE4rskilt n\xE4r de integrerar\
  \ med system d\xE4r XML\u2026"
lastmod: '2024-03-13T22:44:38.275658-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med XML inneb\xE4r att tolka och generera XML-data i Swift. Programmerare\
  \ g\xF6r detta f\xF6r datautbyte, s\xE4rskilt n\xE4r de integrerar med system d\xE4\
  r XML\u2026"
title: Att arbeta med XML
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att tolka och generera XML-data i Swift. Programmerare gör detta för datautbyte, särskilt när de integrerar med system där XML är standardformatet.

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

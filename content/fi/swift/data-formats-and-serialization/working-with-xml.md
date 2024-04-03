---
date: 2024-01-26 04:36:04.182021-07:00
description: "Kuinka: Swift tarjoaa `XMLParser`- ja `XMLDocument`-luokat XML-tietojen\
  \ j\xE4sent\xE4miseen. T\xE4ss\xE4 on p\xE4tk\xE4 yksinkertaisen XML-merkkijonon\
  \ j\xE4sent\xE4miseen."
lastmod: '2024-03-13T22:44:56.932138-06:00'
model: gpt-4-0125-preview
summary: "Swift tarjoaa `XMLParser`- ja `XMLDocument`-luokat XML-tietojen j\xE4sent\xE4\
  miseen."
title: "XML:n k\xE4sittely"
weight: 40
---

## Kuinka:
Swift tarjoaa `XMLParser`- ja `XMLDocument`-luokat XML-tietojen jäsentämiseen. Tässä on pätkä yksinkertaisen XML-merkkijonon jäsentämiseen:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Älä unohda perjantain juhlaa!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // XMLParserDelegate
    parser.parse()
}
```

Voit myös tuottaa XML:ää käyttämällä `XMLDocument`-luokkaa:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Esimerkkituloste:

```xml
<note>
  <to>Tove</to>
</note>
```

## Syväsukellus
XML eli Laajennettava Merkintäkieli on ollut olemassa 90-luvun lopulta lähtien. Se on sanallinen mutta ihmisen luettavissa, mikä tekee siitä sopivan vaihtoehdon monimutkaisten tietorakenteiden kannalta. Swiftin XML-jäsennyskyvyt eivät ole yhtä kehittyneitä kuin Pythonin ElementTree:ssa tai Javan JAXB:ssä, mutta ne ajavat asiansa perustarpeisiin.

Uusissa järjestelmissä usein suositaan vaihtoehtoja kuten JSON, joiden rakenne on kevyempi ja jäsentimet vähemmän monimutkaisia, mutta XML on edelleen merkittävä monissa yritys- ja perintöjärjestelmissä.

Swiftissä XML:n käsittelyn yhteydessä `XMLParser` on suoratoistoon perustuva jäsentäjä, mikä tarkoittaa, että se lukee XML-dokumentin sekventiaalisesti. Suurten XML-tiedostojen kohdalla tämä on muistitehokasta. Jos kuitenkin etsit yksinkertaisuutta ja XML-tietosi on kohtuullisen pieni, `XMLDocument`-luokan käyttäminen voi olla suoraviivaisempi vaihtoehto.

## Katso Myös
- [Applen XML:n jäsentämisopas](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schoolsin XML-opas](https://www.w3schools.com/xml/)

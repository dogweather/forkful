---
date: 2024-01-26 04:36:04.182021-07:00
description: "XML:n k\xE4sittely tarkoittaa XML-tietojen j\xE4sent\xE4mist\xE4 ja\
  \ tuottamista Swiftiss\xE4. Ohjelmoijat tekev\xE4t t\xE4t\xE4 tiedonvaihtoa varten,\
  \ erityisesti silloin, kun\u2026"
lastmod: '2024-02-25T18:49:53.839113-07:00'
model: gpt-4-0125-preview
summary: "XML:n k\xE4sittely tarkoittaa XML-tietojen j\xE4sent\xE4mist\xE4 ja tuottamista\
  \ Swiftiss\xE4. Ohjelmoijat tekev\xE4t t\xE4t\xE4 tiedonvaihtoa varten, erityisesti\
  \ silloin, kun\u2026"
title: "XML:n k\xE4sittely"
---

{{< edit_this_page >}}

## Mitä & Miksi?
XML:n käsittely tarkoittaa XML-tietojen jäsentämistä ja tuottamista Swiftissä. Ohjelmoijat tekevät tätä tiedonvaihtoa varten, erityisesti silloin, kun integroituminen järjestelmiin, joissa XML on standardimuoto, on tarpeen.

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

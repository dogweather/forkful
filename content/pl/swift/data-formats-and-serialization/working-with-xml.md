---
aliases:
- /pl/swift/working-with-xml/
date: 2024-01-26 04:36:01.119861-07:00
description: "Praca z XML w kontek\u015Bcie Swift oznacza analiz\u0119 i generowanie\
  \ danych XML. Programi\u015Bci robi\u0105 to dla wymiany danych, zw\u0142aszcza\
  \ podczas integracji z\u2026"
lastmod: 2024-02-18 23:08:49.976517
model: gpt-4-0125-preview
summary: "Praca z XML w kontek\u015Bcie Swift oznacza analiz\u0119 i generowanie danych\
  \ XML. Programi\u015Bci robi\u0105 to dla wymiany danych, zw\u0142aszcza podczas\
  \ integracji z\u2026"
title: Praca z XML
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z XML w kontekście Swift oznacza analizę i generowanie danych XML. Programiści robią to dla wymiany danych, zwłaszcza podczas integracji z systemami, gdzie XML jest standardowym formatem.

## Jak to zrobić:
Swift zapewnia `XMLParser` oraz `XMLDocument` do analizy danych XML. Oto fragment kodu do analizy prostego ciągu XML:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Przypomnienie</heading>
    <body>Nie zapomnij o imprezie w piątek!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Twój XMLParserDelegate
    parser.parse()
}
```

Możesz też generować XML za pomocą `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Przykładowy wynik:

```xml
<note>
  <to>Tove</to>
</note>
```

## Pogłębiona analiza
XML, czyli Rozszerzalny Język Znaczników, istnieje od końca lat 90. Jest rozwlekły, ale czytelny dla człowieka, co czyni go dobrym wyborem dla skomplikowanych struktur danych. Możliwości analizy XML w Swifcie nie są tak rozbudowane jak te dostępne w Python's ElementTree czy Java's JAXB, ale wystarczają do zaspokojenia podstawowych potrzeb.

Alternatywy, takie jak JSON, są często preferowane w nowych systemach ze względu na ich mniejszą wagę i mniej skomplikowane parsery, ale XML jest nadal obecny w wielu systemach korporacyjnych i starszych.

Pracując z XML w Swifcie, `XMLParser` to parser oparty na strumieniach, co oznacza, że czyta dokument XML sekwencyjnie. Dla dużych plików XML jest to wydajne pod względem pamięci. Jednakże, jeśli szukasz prostoty i Twoje dane XML są raczej małe, użycie `XMLDocument` może być bardziej bezpośrednie.

## Zobacz również
- [Przewodnik po analizie XML od Apple](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [Samouczek XML z W3Schools](https://www.w3schools.com/xml/)

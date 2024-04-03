---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:36.254656-07:00
description: "Hur man g\xF6r: Swift inkluderar som standard inte ett inbyggt bibliotek\
  \ f\xF6r HTML-tolkning, vilket kr\xE4ver anv\xE4ndning av tredjepartsbibliotek f\xF6\
  r att\u2026"
lastmod: '2024-03-13T22:44:38.248949-06:00'
model: gpt-4-0125-preview
summary: "Swift inkluderar som standard inte ett inbyggt bibliotek f\xF6r HTML-tolkning,\
  \ vilket kr\xE4ver anv\xE4ndning av tredjepartsbibliotek f\xF6r att effektivt hantera\
  \ denna uppgift."
title: Tolka HTML
weight: 43
---

## Hur man gör:
Swift inkluderar som standard inte ett inbyggt bibliotek för HTML-tolkning, vilket kräver användning av tredjepartsbibliotek för att effektivt hantera denna uppgift. Ett av de mest populära valen är SwiftSoup, ett rent Swift-bibliotek som erbjuder jQuery-lik syntax för HTML-tolkning och manipulation.

### Installation
Först behöver du lägga till SwiftSoup i ditt projekt. Om du använder Swift Package Manager, kan du lägga till det i dina `Package.swift` beroenden:

```swift
dependencies: [
    .package(url: "https://github.com/scinfu/SwiftSoup.git", från: "2.3.2")
]
```

### Exempel: Extrahera Länkar från HTML
Anta att du har ett HTML-dokument och du vill extrahera alla länkar (`<a href="...">`). Med SwiftSoup kan du enkelt åstadkomma detta:

```swift
import SwiftSoup

let html = """
<!DOCTYPE html>
<html>
<head>
    <title>Exempelsida</title>
</head>
<body>
    <p>Välkommen till vår webbplats</p>
    <a href="https://example.com/page1">Sida 1</a>
    <a href="https://example.com/page2">Sida 2</a>
</body>
</html>
"""

gör {
    let doc: Document = försök SwiftSoup.parse(html)
    let links: Elements = försök doc.select("a")
    för link i links.array() {
        let linkHref: String = försök link.attr("href")
        let linkText: String = försök link.text()
        print("\(linkText) - \(linkHref)")
    }
} fånga Exception.Error(låt typ, låt meddelande) {
    print("Feltyp: \(typ) Meddelande: \(meddelande)")
} fånga {
    print("fel")
}
```

### Exempel på Utdata
Den föregående koden extraherar URL:er och deras text från HTML, och skriver ut:

```
Sida 1 - https://example.com/page1
Sida 2 - https://example.com/page2
```

Detta grundläggande exempel demonstrerar hur man utnyttjar SwiftSoup för att tolka HTML-dokument. Genom att utforska SwiftSoup's dokumentation ytterligare, kan du hitta många metoder för att navigera, söka och modifiera HTML-innehållet, vilket ger dina Swift-applikationer möjligheten att bearbeta komplex webbinnehåll med lätthet.

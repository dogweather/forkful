---
title:                "Hantera HTML"
html_title:           "Swift: Hantera HTML"
simple_title:         "Hantera HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att parsa HTML är en process som innebär att läsa och tolka HTML-kod för att extrahera olika element och data. Detta är en vanlig uppgift för programmerare som arbetar med webbutveckling, automatisk webbskrapning eller dataanalys. Genom att parsa HTML kan man både automatisera och effektivisera många uppgifter som annars skulle vara mycket tidskrävande att utföra manuellt.

## Så här gör du: 
Ett enkelt sätt att parsa HTML i Swift är att använda biblioteket SwiftSoup. Nedan följer ett exempel på hur man kan använda detta bibliotek för att hämta och skriva ut titlarna på alla länkar på en viss webbsida:

```Swift
import SwiftSoup

// Hämta en HTML-sida som en sträng
let html = "<html><head><title>Min hemsida</title></head><body><a href="https://www.example.com">Länk 1</a><a href="https://www.example.com">Länk 2</a></body></html>"

do {
    // Skapa ett SwiftSoup-document från HTML-strängen
    let doc = try SwiftSoup.parse(html)
    
    // Hämta alla länkar från HTML-dokumentet
    let links = try doc.select("a")
    
    // Loopa igenom alla länkar och skriv ut titlarna
    for link in links.array() {
        print(try link.text())
    }
    
} catch Exception {

    print("Något gick fel: \(error)")
}
```

Detta exempel visar hur man kan använda SwiftSoup för att extrahera data från en HTML-sida. För mer avancerade användningsområden, som att filtrera och manipulera HTML-kod, finns det också andra bibliotek och verktyg tillgängliga.

## Djupdykning: 
Parsing av HTML är en vanlig uppgift inom webbutveckling eftersom det är ett enkelt och effektivt sätt att hämta och använda data från webbsidor. Historiskt sett har detta varit en ganska komplicerad uppgift i Swift, men med tillkomsten av olika bibliotek och ramverk, som SwiftSoup, har det blivit betydligt enklare att implementera.

Det finns också andra sätt att hantera HTML-kod, som att använda reguljära uttryck (regular expressions) eller bygga ett eget parser från grunden. Men dessa metoder är oftast mer komplexa och tar längre tid att implementera jämfört med att använda ett färdigt bibliotek som SwiftSoup.

## Se även: 
- [SwiftSoup dokumentation](https://github.com/scinfu/SwiftSoup)
- [Web scraping med Swift: En guide](https://medium.com/@webdesign_team/introduction-to-web-scraping-with-swift-7884febea62d)
- [Reguljära uttryck i Swift](https://medium.com/better-programming/regular-expressions-in-swift-b1fe5366a190)
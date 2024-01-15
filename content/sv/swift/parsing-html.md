---
title:                "Utvinna html"
html_title:           "Swift: Utvinna html"
simple_title:         "Utvinna html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Att parsam HTML kan vara en användbar färdighet för att kunna hämta och extrahera data från webbsidor. Det kan vara användbart för webbskrapning, dataskydd eller att bara behandla webbdata på ett effektivt sätt.

## Så här gör du

För att börja pars HTML i Swift, behöver du en HTML-parser-bibliotek. Ett populärt val är SwiftSoup, som är en Swift-implementering av Jsoup, en välkänd Java-baserad HTML-parser.

Du kan installera SwiftSoup via Swift Package Manager genom att lägga till följande rad i din Package.swift fil:

```Swift 
.package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.0.0")
```

När du har installerat SwiftSoup kan du börja pars HTML genom att skapa en `Document`-instans och ladda in innehållet från din webbsida. Du kan sedan använda `getElementsByTag()` för att hämta alla element med en viss tagg, t.ex. `p` för att hämta alla `<p>`-taggar.

```Swift
do {
  let html = "<html><head><title>Min hemsida</title></head><body><p>Välkommen till min hemsida</p></body></html>"
  let doc = try SwiftSoup.parse(html)
  
  let paragraphs = try doc.getElementsByTag("p")
  for p in paragraphs {
    print(try p.text())
  }
} catch {
  print("Kunde inte göra HTML-parsing: \(error)")
}
```

Output: 
```
Välkommen till min hemsida
```

## Djupdykning

När du gjort lite grundläggande parsing av HTML, kan du börja utforska mer avancerade funktioner. SwiftSoup stödjer CSS-querys, så du kan hämta element med specifika klasser, id:n eller attribut.

Du kan också använda `select()` för att söka efter element med en viss CSS-selector. Till exempel kan du söka efter alla länkar (`<a>`-taggar) som har en `href`-attribut som börjar med "https://www.".

```Swift
let links = try doc.select("a[href^=https://www.]")
```

SwiftSoup har också stöd för att ändra HTML-innehållet genom att lägga till, ta bort eller redigera element och attribut. Du kan också konvertera en `Document` till en `String` eller `Data` för att spara eller skicka den vidare.

## Se även

- [SwiftSoup dokumentation](https://github.com/scinfu/SwiftSoup)
- [HTML-parsing med Swift: en snabb introduktion](https://medium.com/@sergalbop/html-parsing-with-swift-a-quick-introduction-807baf8f6a3a)
- [Moderna Cocoa-tipstackar: HTML-parsing med Swift](https://www.raywenderlich.com/141285/modern-cocoa-tutorials-html-parsing-swift)
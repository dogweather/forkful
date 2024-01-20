---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/parsing-html.md"
---

{{< edit_this_page >}}

# Parse HTML med Swift

## Vad & Varför?

HTML-parning är processen att omvandla HTML-data till någon som programmet kan förstå. Programmerare gör detta för att interagera med och extrahera data från HTML-webbsidor.

## Så här gör du:

Med Swift kan vi använda SwiftSoup-biblioteket för att enkelt parsa HTML. Här är ett exempel:

```Swift
import SwiftSoup

do {
    let html = "<html><head><title>Enkel webbsida</title></head><body>Välkommen till min webbsida</body></html>"
    
    let doc: Document = try SwiftSoup.parse(html)

    let title: Element = try doc.select("title").first()!
    print(try title.text())
    
    let body: Element = try doc.select("body").first()!
    print(try body.text())
} catch Exception.Error(let type, let message) {
    print(message)
} catch {
    print("catch error")
}
```

När du kör ovanstående kod skriver den ut:

```Swift
Enkel webbsida
Välkommen till min webbsida
```

## Fördjupning

Historiskt sett var HTML parning inte lika enkel som det är idag. Utvecklare måste manuellt navigera i HTML-dokument, vilket var tidskrävande och öppen för fel. Idag, tack vare bibliotek som SwiftSoup, kan vi enkelt parsa HTML.

Men det finns också alternativ till SwiftSoup, till exempel Kanna-biblioteket. Alternativen kan variera beroende på vad du prövar att uppnå. Förståelse av dina behov hjälper dig att välja det mest lämpliga biblioteket. 

Vad gäller implementations detaljer, använder SwiftSoup CSS-väljare för att hjälpa dig att hitta elementen på HTML-sidan. Det gör det lättare att extrahera de data som du behöver.

## Se även

För mer information om SwiftSoup, inklusive dokumentation och mer exempel, se följande resurser:

- [SwiftSoup GitHub](https://github.com/scinfu/SwiftSoup)
- [Användning av SwiftSoup](https://medium.com/better-programming/swiftsoup-html-parsing-like-javascript-in-swift-1973d79ffb67)

För alternativa bibliotek för HTML parsning i Swift, överväga att läsa om:

- [Kanna-biblioteket](https://github.com/tid-kijyun/Kanna)
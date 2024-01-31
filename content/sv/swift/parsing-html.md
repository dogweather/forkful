---
title:                "Tolka HTML"
date:                  2024-01-20T15:34:13.889327-07:00
simple_title:         "Tolka HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing av HTML innebär att vi tolkar och omvandlar HTML-kod för att kunna hantera dess innehåll programmässigt. Programmerare gör detta för att extrahera data, manipulera innehåll, eller för att integrera webbinnehåll i appar.

## Hur man gör:
Du kan använda ett bibliotek som SwiftSoup för att enkelt hantera HTML. Här är ett grundläggande exempel:

```Swift
import SwiftSoup

do {
    let html = "<html><head><title>Första Exemplet</title></head><body><p>Hej Swift!</p></body></html>"
    let doc = try SwiftSoup.parse(html)
    let bodyText = try doc.text()
    print(bodyText)  // Output: "Hej Swift!"
} catch Exception.Error(let type, let message) {
    print("Message: \(message)")
} catch {
    print("error")
}
```

## Fördjupning:
HTML-parsing är inte nytt – det har varit en del av webbutveckling sedan webbens födelse. Alternativ till SwiftSoup inkluderar andra bibliotek som Kanna eller till och med en manuell tillvägagångssätt via regex, men det senare är inte rekommenderat på grund av HTML:s komplexa natur.

SwiftSoup, inspirerat av Java-biblioteket Jsoup, tillhandahåller Swiftnära syntax och funktionalitet. Implementeringen använder intern parsinglogik som omvandlar HTML till en DOM-struktur som sedan kan sökas igenom och manipuleras. Detta abstraherar komplexiteten kring HTML-struktur och gör det programmerarvänligt.

## Se Även:
- SwiftSoup GitHub: https://github.com/scinfu/SwiftSoup
- Kanna GitHub: https://github.com/tid-kijyun/Kanna
- W3Schools HTML Parser: https://www.w3schools.com/xml/dom_intro.asp

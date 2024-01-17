---
title:                "Omvandla en sträng till gemener"
html_title:           "Swift: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att ändra alla stora bokstäver i en sträng till små bokstäver. Detta är användbart för att jämföra strängar och söka efter specifika ord eller tecken. Programmerare använder denna funktion för att göra sökningar mer flexibla och för att separera ord som annars skulle anses som olika på grund av storleken på bokstäverna.

## Hur:
För att konvertera en sträng till gemener i Swift, använd funktionen `lowercased()` på den aktuella strängen. Till exempel:

```Swift
let myString = "HEJ ALLA!"

print(myString.lowercased())
// output: "hej alla!"
```

## Djupgående:
Förutom att använda `lowercased()` finns det andra sätt att konvertera en sträng till gemener, som att använda funktionen `lowercaseString` från NSString-klassen. Historiskt sett har denna funktion använts för att bevara kompatibilitet med Objective-C-kod. Implementeringsdetaljer inkluderar möjligheten att behålla språkspecifika eller accentuerade tecken vid konvertering till gemener.

## Se även:
- [Swift Standard Library - String](https://developer.apple.com/documentation/swift/string)
- [Objective-C NSString Class Reference](https://developer.apple.com/documentation/foundation/nsstring)
---
title:    "Swift: Storletterstring"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## För Att: Varför du skulle vilja använda storstavning i en sträng

När du jobbar med text i en applikation, kanske du ibland vill ändra utseendet på en sträng genom att göra den storstavad, det vill säga ha stora bokstäver. Det kan vara för att göra den mer framträdande eller för att matcha en viss design. Oavsett anledning, är det enkelt att göra med Swifts inbyggda funktioner.

## Hur Man Gör Det: 

För att kunna storstava en sträng i Swift, behöver vi använda funktionen `uppercased()` på vår sträng. Det ser ut så här:

```Swift
let sträng = "hej, jag är en string"
print(sträng.uppercased())
```

Detta kommer att ge följande output: "HEJ, JAG ÄR EN STRING". Som du kan se, konverterar `uppercased()`funktionen alla tecken i vår sträng till stora bokstäver.

## Djupdykning: 

Det finns även andra sätt att storstava en sträng i Swift, till exempel genom att använda `capitalized()` eller `localizedUppercase`. Det är viktigt att notera att `uppercased()` endast fungerar för strängar som innehåller latinska bokstäver, medan de andra funktionerna fungerar för ett bredare utbud av språk och teckenuppsättningar.

Det finns också möjlighet att anpassa storstavningen genom att använda en `Locale` parameter för de olika funktionerna. Detta gör det möjligt att hantera specifika tecken och teckenuppsättningar som används i vårt språk.

## Se även: 

* [Officiell Swift dokumentation för strängar](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
* [Apple Developer Documentation för storstavning av strängar i Swift](https://developer.apple.com/documentation/swift/1970737-uppercased)
---
title:                "Att konvertera en sträng till versaler"
html_title:           "Swift: Att konvertera en sträng till versaler"
simple_title:         "Att konvertera en sträng till versaler"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng i Swift är ett vanligt behov när man jobbar med textdata. Det är ett enkelt sätt att få en sträng att se mer formell och enhetlig ut, till exempel om den ska visas i en rubrik eller ett meddelande.

## Så här gör du

Kapitalisera en sträng i Swift är enkelt och kan göras på flera olika sätt beroende på dina behov. Här är några exempel på olika metoder:

```Swift
// Enklaste sättet, genom att använda den inbyggda metoden .uppercased():
let namn = "anna"
let kapitaliseratNamn = namn.uppercased()
print(kapitaliseratNamn) // Resultat: ANNA

// Om du vill ha stora bokstäver för varje ord, använd .capitalized:
let fras = "jag älskar swift"
let kapitaliseradFras = fras.capitalized
print(kapitaliseradFras) // Resultat: Jag Älskar Swift

// Om du bara vill ha första bokstaven kapitaliserad, använd .capitalizedFirstLetter():
let mening = "hej, välkommen till Swift"
let kapitaliseradMening = mening.capitalizedFirstLetter()
print(kapitaliseradMening) // Resultat: Hej, välkommen till Swift
```

## Djupdykning

I Swift finns det flera inbyggda metoder för att hantera strängar, inklusive de som används för att kapitalisera en sträng. Det är viktigt att notera att dessa metoder skapar en helt ny sträng istället för att ändra den befintliga strängen direkt. Detta beror på att strängar i Swift är "value types" istället för "reference types", vilket innebär att de är oföränderliga.

Det finns också möjlighet att använda sig av externa bibliotek som erbjuder ytterligare funktioner för att hantera och formatera textdata. Ett exempel på ett sådant bibliotek är SwiftString, som erbjuder en mängd olika metoder för att omvandla och manipulera strängar.

## Se även

Här är några länkar till ytterligare resurser och guider för att arbeta med strängar i Swift:

- [The Swift Programming Language - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Ray Wenderlich - Strings in Swift - Ultimate Guide](https://www.raywenderlich.com/3447256-strings-in-swift)
- [SwiftString - A lightweight string extension for Swift](https://github.com/amayne/SwiftString)
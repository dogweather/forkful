---
title:                "Konvertera en sträng till gemener"
html_title:           "Swift: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till små bokstäver är en vanlig uppgift i programmering, oavsett om det är för att få en enhetlig formatering eller för att jämföra strängar utan att bry sig om stora och små bokstäver. Det finns flera olika metoder i Swift som kan användas för att utföra detta.

## Hur man gör det
```Swift
let sträng = "HELLO SWIFT"
print(sträng.lowercased()) // Skriver ut "hello swift"
```

Den enklaste metoden för att konvertera en sträng till små bokstäver är genom att använda metoden `lowercased()`, som finns tillgänglig på alla `String`-objekt i Swift. Detta kommer att returnera en ny sträng med alla bokstäver i små bokstäver.

```Swift
let sträng = "Det här är En Sträng"
print(sträng.lowercased()) // Skriver ut "det här är en sträng"
```

Om du behöver konvertera en sträng till små bokstäver utan att göra några förändringar i den ursprungliga strängen kan du använda metoden `lowercased()` tillsammans med `map()` för att applicera denna konvertering på varje tecken i strängen.

```Swift
let sträng = "ExEMPl 123 !!!"
let småbokstäver = sträng.lowercased().map {String($0)}
print(småbokstäver) // Skriver ut ["e", "x", "e", "m", "p", "l", " ", "1", "2", "3", " ", "!", "!", "!"]
```

## Djupgående
Det finns flera andra metoder som kan användas för att konvertera en sträng till små bokstäver, till exempel `localizedLowercase`, som tar hänsyn till språkinställningar. Det finns också vissa skillnader i hur dessa metoder hanterar speciella tecken som å, ä och ö. Du kan läsa mer om detta i Swifts officiella dokumentation om `String`-objektet.

## Se även
- [Officiell Swift dokumentation för String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID285)
- [Stack Overflow-fråga om att konvertera en sträng till små bokstäver i Swift](https://stackoverflow.com/questions/26508485/how-to-make-string-lowercase-in-swift)
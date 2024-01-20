---
title:                "Interpolering av en sträng"
html_title:           "Arduino: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Stränginterpolering är tekniken att injicera variabler direkt i en sträng. Programmerare gör detta för att skapa dynamiska strängar utan att behöva slå samman delsträngar.

## Så gör du:

Här är ett basexempel på hur du använder stränginterpolering i Swift:

```Swift
let name = "Erik"
let greeting = "Hej, \(name)!"
print(greeting)  // Utfallet: "Hej, Erik!"
```

Det här är ett något mer komplicerat exempel:

```Swift
let hoursWorked = 9
let hourlyRate = 100.5
let earnings = "Du tjänade \(hoursWorked * hourlyRate) kr idag."
print(earnings)  // Utfallet: "Du tjänade 904.5 kr idag."
```
Med stränginterpolering kan du infoga variabelvärden, uttryck eller även resultat av funktioner direkt i en sträng.

## Fördjupning

Stränginterpolering i Swift har rötter i tidigare programmeringsspråk som C och JavaScript, men Swift's implementering av stränginterpolering är mer kraftfull och flexibel än de tidigare versionerna.

Som alternativ till stränginterpolering finns det också gammaldags strängsammanfogning. Men det kan snabbt bli klumpigt och svårt att läsa, särskilt med större strängar och fler variabler.

Vid stränginterpolering översätter Swift-kompilatorn din interpolerade sträng till en serie strängsegment och uttryckssegment. Uttryckssegmenten utvärderas och omvandlas till strängar, som sedan sammanfogas med strängsegmenten.

## Se även:

För mer detaljerad information, ta en titt på dessa resurser:

- Swift's dokumentation om stränginterpolering [här](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- En djupgående artikel om stränginterpolering i Swift på [Swift by Sundell](https://www.swiftbysundell.com/articles/string-interpolation-in-swift/)
- En diskussion om effektiviteten av stränginterpolering mot strängsammanfogning på [Stack Overflow](https://stackoverflow.com/questions/51017084/swift-string-interpolation-vs-string-concatenation-efficiency)
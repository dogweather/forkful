---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

# Swift Programmering: Skriv ut Debug Output

## Vad & Varför?

Att skriva ut debug output är det snabbaste sättet att följa din kod under körning. Med det kan vi hitta buggar, better förstå hur koden exekverar, plus verifiera att datastrukturer är korrekt uppförda och begränsas.

## Hur gör man:

Härunder hittar du hur du kan skriva ut i konsolen med Swift.

```Swift
// Enkel output till konsolen
print("Hej, Swift!")

// Output med variabel
var nummer = 10
print("Numret är \(nummer)")

// Debug Description
dump(nummer)
```

Om du kör detta kod kommer du att se följande output:

```
Hej, Swift!
Numret är 10
10
```
## Djupdykning

Historiskt har utvecklare alltid behövt sätt att övervaka ett programs tillstånd, och skriva ut debug-output är ett av de mest grundläggande verktygen i deras arsenal. Det är direkt arv från tidigare programmeringsspråk som C och JavaScript.

Vad gäller alternativ, kan du använda debuggers, som är mer kraftfulla men kräver mer inlärning. Swifts inbyggda `dump()` funktion Printing debug output är dock snabbt, lättanvänt och universellt, vilket gör det till ett go-to verktyg för många utvecklare.

Intressant i Swift är att standard `print()` funktion inte visar alla detaljer för komplexa objekt, men `dump()` gör, vilket gör det till en värdefull tillägg i en Swift-utvecklares verktygslåda.

## Se också

För mer information om att skriva ut debug output och debugging i Swift, se följande källor:

- Apple's officiella [Swift dokumentation](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Debugging with Xcode](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/debugging_tools.html)
- [Använda print och Dump i Swift](https://www.hackingwithswift.com/example-code/language/how-to-use-dump-to-debug-your-swift-code) från Hacking With Swift.
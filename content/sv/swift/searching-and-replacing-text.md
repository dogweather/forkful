---
title:                "Söka och ersätta text"
html_title:           "Swift: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att kunna söka och ersätta text i en kodbas är en viktig och användbar funktion för Swift-utvecklare. Det kan hjälpa till att snabbt göra ändringar i koden och minska risken för skrivfel.

## Så här gör du

För att söka och ersätta text i Swift, använd funktionen `replacingOccurrences(of:with:)`. Här är ett exempel på hur du kan ersätta alla förekomster av ett ord med ett annat i en sträng:

```Swift
let str = "Jag älskar att koda i Swift!"
let nyStr = str.replacingOccurrences(of: "älskar", with: "tycker om")

print(nyStr) // Output: "Jag tycker om att koda i Swift!"
```

Om du vill söka och ersätta text i en hel kodbas kan du använda Xcode's "Find and Replace" -funktion. Du kan söka efter ett specifikt ord och välja att ersätta det i hela projektet eller bara i vissa filer.

## Djupdykning

När du använder `replacingOccurrences(of:with:)`-funktionen, kommer alla förekomster av det sökta ordet att ersättas. Detta inkluderar även om ordet finns som en del av ett annat ord.

I exemplet ovan kommer "älskar" att ersättas även om det finns som en del av ordet "älskarinnan". För att undvika detta kan du använda `replacingOccurrences(of:with:options:range:)` och specificera vilka delar av strängen som ska sökas i för att undvika oönskade ersättningar.

Det finns också andra funktioner som kan komma till nytta vid sökning och ersättning av text, som `range(of:)` för att hitta en del av en sträng och `replacingCharacters(in:with:)` för att ersätta en del av en sträng med en annan.

## Se också

- [Swift's String Dokumentation](https://developer.apple.com/documentation/swift/string)
- [How to use Find and Replace in Xcode](https://medium.com/@twannl/xcode-find-and-replace-explained-5c1e8c1ef4b1)
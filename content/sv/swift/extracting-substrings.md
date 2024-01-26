---
title:                "Extrahera delsträngar"
date:                  2024-01-20T17:46:44.439511-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Extrahering av substrängar involverar att plocka specifika delar ur en sträng, något som ofta nyttjas för att bearbeta textdata efter behov. Det sparas tid och ökar programmets effektivitet när endast relevant textdata hanteras.

## Hur gör man?:
Extrahera en substräng med hjälp av Swift kan göras enkelt med `String` metoder och `Range` objekt. Här är ett snabbt exempel:

```Swift
let fullString = "Hej, Swift-programmerare!"
if let range = fullString.range(of: "Swift") {
    let substring = fullString[range]
    print(substring)  // "Swift"
}
```

Resultat:
```
Swift
```

Du kan även använda index direkt för att klippa ut delar:

```Swift
let startIndex = fullString.index(fullString.startIndex, offsetBy: 5)
let endIndex = fullString.index(startIndex, offsetBy: 5)
let substring = fullString[startIndex..<endIndex]

print(substring) // "Swift"
```

Resultat igen blir:
```
Swift
```

## Djupdykning:
Substrängar har varit en del av programmering sedan de första strängmanipuleringsförmågorna introducerades. I Swift har behandlingen av substrängar blivit mer effektiv genom användandet av `String` och `Substring` typen som delar samma underliggande minne. Före Swift 4, manipulerades strängar annorlunda som kunde orsaka prestandaproblem. Alternativ till inbyggda Swift metoder inkluderar reguljära uttryck och använder tredjepartsbibliotek som kan hantera mer avancerade textprocessningsbehov.

## Se även:
- Swift-dokumentation om strängar och tecken: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Apple Developer's String Programming Guide: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/)
- En introduktion till reguljära uttryck i Swift: [https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started)

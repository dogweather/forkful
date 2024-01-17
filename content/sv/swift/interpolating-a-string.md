---
title:                "Interpolera en sträng"
html_title:           "Swift: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av en sträng i Swift är när man sätter variabler eller uttryck inuti en sträng, så att den kan anpassas dynamiskt. Programerare använder detta för att enkelt konstruera strängar som behöver ändras baserat på olika variabler eller värden.

## Så här gör du:
Det enklaste sättet att interpolera en sträng i Swift är genom att använda formattering med hjälp av `\( )` syntax. Detta låter dig infoga värden eller uttryck inne i en sträng. Till exempel:

```Swift
let name = "Lisa"
let age = 25
let greeting = "Hej, mitt namn är \(name) och jag är \(age) år gammal."
print(greeting)
```
Output: Hej, mitt namn är Lisa och jag är 25 år gammal.

## Utforska Djupet:
Interpolering av strängar har funnits sedan C-programmeringsspråket på 1970-talet, men det blev inte populärt förrän senare. Innan interpolering var det vanligt att använda funktioner som `sprintf` för att konstruera dynamiska strängar.

Men i Swift är interpolering integrerat direkt i språket och gör det mycket enklare för programmerare att konstruera dynamiska strängar. Andra alternativ inkluderar konkatenering av strängar eller användning av formateringsmetoder som `String(format: )`.

Implementation av interpolering i Swift görs genom att språket analyserar strängen och identifierar variabler eller uttryck i `\( )` syntax. Den här processen händer i bakgrunden och gör det enkelt för oss att använda interpolering i vår kod.

## Se även:
- [Apple Swift dokumentation om stränginterpolering](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Ray Wenderlich's tutorial on string interpolation in Swift](https://www.raywenderlich.com/130197/swift-string-interpolation)
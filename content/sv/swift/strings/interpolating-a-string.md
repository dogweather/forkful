---
date: 2024-01-20 17:51:35.717719-07:00
description: "Interpolering av str\xE4ngar i Swift inneb\xE4r att du blandar variabler,\
  \ konstanter, litteraler och uttryck inuti en str\xE4ng. Det g\xF6r kod mer l\xE4\
  sbar och\u2026"
lastmod: '2024-03-13T22:44:38.237654-06:00'
model: gpt-4-1106-preview
summary: "Interpolering av str\xE4ngar i Swift inneb\xE4r att du blandar variabler,\
  \ konstanter, litteraler och uttryck inuti en str\xE4ng. Det g\xF6r kod mer l\xE4\
  sbar och\u2026"
title: "Interpolera en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av strängar i Swift innebär att du blandar variabler, konstanter, litteraler och uttryck inuti en sträng. Det gör kod mer läsbar och dynamisk, perfekt för att skapa personligt anpassade meddelanden eller för att sammanfoga data på ett smidigt sätt.

## Hur man gör:
```Swift
let name = "Erik"
let age = 29
let greeting = "Hej \(name), du är \(age) år gammal."
print(greeting)
```
Utskrift: `Hej Erik, du är 29 år gammal.`

```Swift
let price = 109.99
let item = "hörlurar"
let priceMessage = "Priset på \(item) är \(price) kronor inklusive moms."
print(priceMessage)
```
Utskrift: `Priset på hörlurar är 109.99 kronor inklusive moms.`

## Djupdykning
Interpolering introducerades i Swift för att ersätta mer klumpig och felbenägen strängsammansättning, som fanns i tidigare programmeringsspråk som Objective-C. Alternativ till interpolering inkluderar konkatenering med `+` och använder `String(format:)` för mer komplexa format. Under huven ersätter Swift-interpolering platshållare med deras motsvarande strängvärden vid körning, vilket fusionerar dem till en ny sträng.

## Se även
- [Swift Documentation on String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift API Reference on String](https://developer.apple.com/documentation/swift/string)

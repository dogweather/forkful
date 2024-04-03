---
date: 2024-01-20 17:39:10.145203-07:00
description: "S\xE5 h\xE4r g\xF6r du: ."
lastmod: '2024-03-13T22:44:38.238573-06:00'
model: gpt-4-1106-preview
summary: .
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

## Så här gör du:
```Swift
let originalString = "Hej Världen!"
let lowercasedString = originalString.lowercased()
print(lowercasedString) // "hej världen!"
```
Exemplet visar en sträng "Hej Världen!" som konverteras till "hej världen!".

## Fördjupning
Förr i tiden, när datorsystem var mindre standardiserade, kunde behandling av stora och små bokstäver ställa till problem. Olika system hanterade tecken på sitt sätt. Genom att använda gemener kan vi nu standardisera strängdata vilket är särskilt användbart vid textbearbetning och programmering.

Ett alternativ till `.lowercased()` är `.uppercased()` vilket gör motsatsen; konverterar alla tecken i en sträng till versaler. Det finns också lokaliseringsspecifika funktioner för att konvertera tecken, till exempel `lowercased(with: Locale)`, vilket kan vara viktigt för språk med unika tecken.

Implementationen av `.lowercased()` i Swift är rättfram och använder Unicode-standarden för att se till att även tecken utanför det engelska alfabetet hanteras korrekt.

## Se även
- Swift-dokumentationen för String-handling: [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- Unicode-standard: [Unicode Standard](http://www.unicode.org/standard/standard.html)
- Swift-programmeringsguiden: [The Swift Programming Language (Swift 5.7)](https://docs.swift.org/swift-book/)

---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:13.150858-07:00
description: 'Hoe te: In Swift krijg je de lengte van een string door toegang te krijgen
  tot de `count` eigenschap. Eenvoudig, laten we het doen.'
lastmod: '2024-03-13T22:44:51.147372-06:00'
model: gpt-4-0125-preview
summary: In Swift krijg je de lengte van een string door toegang te krijgen tot de
  `count` eigenschap.
title: De lengte van een string vinden
weight: 7
---

## Hoe te:
In Swift krijg je de lengte van een string door toegang te krijgen tot de `count` eigenschap. Eenvoudig, laten we het doen:

```Swift
let groet = "Hallo, Wereld!"
print(groet.count) // Uitvoer: 13
```

Onthoud dat Swift emoji als enkele tekens beschouwt, dankzij Unicode:

```Swift
let zwaai = "👋"
print(zwaai.count)  // Uitvoer: 1
```

## Diepere Duik
Terug in de Objective-C dagen, was de lengte van een string niet zo direct—er was `length` en `lengthOfBytes(using:)`. Swift heeft het netter gemaakt met `count`.

Wees je bewust van samengestelde tekens: visueel enkele tekens gemaakt van meerdere Unicode scalars. `count` handelt deze gracieus af.

Alternatieven? Zeker, je zou door de string kunnen lopen met een lus, maar dat is het wiel opnieuw uitvinden en minder efficiënt.

Onder de motorkap is `count` O(n), waarbij ‘n’ het aantal tekens is. Dat komt omdat Swift’s `String` geen verzameling van `Char`s is, maar een reeks van grapheme clusters, die kunnen variëren in lengte.

## Zie Ook
- Swift Documentatie over Strings: [Swift String Docs](https://developer.apple.com/documentation/swift/string)
- Basisprincipes van Unicode: [Unicode Consortium](https://home.unicode.org)
- Duik in Swift’s String Performance: [Swift String Perf](https://swift.org/blog/utf8-string/)

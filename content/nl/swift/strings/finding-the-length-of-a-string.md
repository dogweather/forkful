---
title:                "De lengte van een string vinden"
aliases:
- /nl/swift/finding-the-length-of-a-string/
date:                  2024-01-28T22:00:13.150858-07:00
model:                 gpt-4-0125-preview
simple_title:         "De lengte van een string vinden"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
De lengte van een string vinden betekent uitzoeken hoeveel tekens deze bevat. Programmeurs doen dit om invoer te valideren, tekst te manipuleren, of simpelweg om de grootte van hun gegevens te begrijpen.

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

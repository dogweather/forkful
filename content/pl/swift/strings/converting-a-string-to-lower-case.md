---
date: 2024-01-20 17:39:10.267180-07:00
description: "How to: (Jak to zrobi\u0107:) Rozwa\u017Cmy przyk\u0142adowy string\
  \ i jego konwersj\u0119 do ma\u0142ych liter w Swift."
lastmod: '2024-03-13T22:44:35.741954-06:00'
model: gpt-4-1106-preview
summary: "Rozwa\u017Cmy przyk\u0142adowy string i jego konwersj\u0119 do ma\u0142\
  ych liter w Swift."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## How to: (Jak to zrobić:)
Rozważmy przykładowy string i jego konwersję do małych liter w Swift:

```Swift
let exampleString = "DzIeń DoBrY!"
let lowercasedString = exampleString.lowercased()
print(lowercasedString)
```

Wynik działania kodu:

```
dzień dobry!
```

Jest prosto, prawda?

## Deep Dive (Dogłębna analiza)
Po pierwsze, konwersja na małe litery ma długą historię w programowaniu – już pierwsze systemy informatyczne traktowały tekst, gdzie różnica między wielkimi a małymi literami mogła powodować błędy.

Alternatywnie, możesz użyć innych metod jak `localizedLowercase` dla uwzględnienia specyficznych zasad danego języka:

```Swift
let polishString = "Łódź"
print(polishString.localizedLowercase)
```

Warto zauważyć, że konwersja jest kwestią Unicode i działanie `lowercased()` może być inaczej zaimplementowane w zależności od języka.

## See Also (Zobacz również)
- Wikipedia o [Unicode Case Folding](https://en.wikipedia.org/wiki/Unicode_equivalence#Case_folding)
- Porównanie `lowercased()` i `localizedLowercase` [String Transformation in Swift](https://www.hackingwithswift.com/articles/141/8-powerful-swift-string-functions)

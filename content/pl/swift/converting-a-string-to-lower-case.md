---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-01-20T17:39:10.267180-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Zmiana stringa na małe litery to prosty proces, gdzie wszystkie litery w napisie są konwertowane na ich małe odpowiedniki. Programiści robią to, by ujednolicić dane - przydatne, gdy porównujesz napisy czy sortujesz dane.

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

---
title:                "Wyszukiwanie i zamienianie tekstu"
html_title:           "Swift: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Czym jest wyszukiwanie i zamiana tekstu & Dlaczego to robimy?
Wyszukiwanie i zamiana tekstu to jedna z podstawowych czynności programistycznych, polegająca na znalezieniu i podmianie określonego ciągu znaków w tekście. Programiści często wykonują tę czynność, aby automatycznie poprawić błędy ortograficzne, zmienić formatowanie lub zaktualizować przestarzały kod.

## Jak to zrobić? 
Możesz użyć funkcji ```replacingOccurrences(of:with:)``` aby wyszukać i zamienić tekst wewnątrz innego tekstu. Na przykład, jeśli chcesz zamienić wszystkie wystąpienia słowa "kot" na "pies" w zdaniu "Lubię koty", użyjemy następującego kodu:

```Swift
let zdanie = "Lubię koty"
let noweZdanie = zdanie.replacingOccurrences(of: "kot", with: "pies")
print(noweZdanie) // Output: Lubię psy
 ```
 
 ## Głębsza analiza
Wyszukiwanie i zamiana tekstu jest jedną z najważniejszych funkcji w procesie przetwarzania tekstów. W przeszłości, wymagało to użycia wyrażeń regularnych lub ręcznego sprawdzania i podmieniania każdego wystąpienia tekstu. Jednak dzięki funkcji ```replacingOccurrences(of:with:)```, możemy wygodnie i efektywnie wykonać tę czynność.

## Zobacz także
Jeśli chcesz dowiedzieć się więcej o wyszukiwaniu i podmianie tekstu w Swift, możesz przeczytać dokumentację Apple na ten temat: https://developer.apple.com/documentation/foundation/nsstring/1412186-replacingoccurrences. Możesz również wypróbować inne metody podmiany tekstu, takie jak funkcja ```replacingCharacters(in:with:)```, aby rozszerzyć swoje umiejętności programistyczne.
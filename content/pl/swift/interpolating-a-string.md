---
title:                "Interpolacja ciągu znaków"
html_title:           "Swift: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interpolowanie stringów to proces łączenia stałych ciągów znaków z zmiennymi do tworzenia nowego stringa. Programiści używają go, aby dynamicznie wstawiać wartości zmiennych do swoich stringów.

## Jak to zrobić:
```Swift
let name = "Anna"
let age = 25
let message = "Cześć, nazywam się \(name) i mam \(age) lat!"
print(message)
```
**Wynik:**
> Cześć, nazywam się Anna i mam 25 lat!

## Wnikliwsze spojrzenie:
Interpolowanie stringów jest powszechną praktyką od lat, ale dopiero Swift wprowadził prostą składnię z użyciem symbolu "\". Alternatywnym sposobem jest łączenie stałych stringów i zmiennych za pomocą operatora "+". Implementacja interpolacji w Swift jest wydajniejsza, ponieważ unika narzutów związanych z alokacją pamięci dla stałych stringów i konwersją zmiennych na stringi.

## Zobacz także:
Dla szczegółowych informacji o interpolacji stringów w języku Swift, polecam przeczytać artykuł na stronie [Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293).
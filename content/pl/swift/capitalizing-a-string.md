---
title:                "Zamiana liter w napisie na wielkie"
html_title:           "Swift: Zamiana liter w napisie na wielkie"
simple_title:         "Zamiana liter w napisie na wielkie"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Zamienianie na wielkie litery, inaczej capitalizing, polega na konwersji wszystkich małych liter ciągu (string) na wielkie. Programista robi to, by podkreślić istotne fragmenty tekstu lub uczynić go bardziej czytelnym.

## Jak to zrobić:
Swift oferuje bardzo prostą metodę na capitalizację stringów:

```Swift
let nazwa = "programowanie"
let nazwaCapitalized = nazwa.capitalized
print(nazwaCapitalized)  // Wypisze: Programowanie
```
W powyższym kodzie, `capitalized` to wbudowana właściwość w Swift, która zamienia pierwszą literę każdego słowa w ciągu na wielką.

## Głębsze spojrzenie:
Początkowo, w wielu innych językach programowania, konwersja na wielkie litery wynikała głównie z konieczności wyróżnienia tytułów czy nagłówków. W Swift, zastosowanie `capitalized` ułatwia nam to zadanie. Jeżeli jednak chcemy zamienić **WSZYSTKIE** litery na duże, użyjemy `uppercased()`:

```Swift 
let nazwaDuzeLitery = nazwa.uppercased()
print(nazwaDuzeLitery)  // Wypisze: PROGRAMOWANIE
```
Dodatkowo, warto zauważyć, że funkcja `capitalized` działa zgodnie z ustawieniami lokalnymi, np. obsługuje niemieckie umlaute i tureckie litery 'i'.
Jednak na każdą funkcję musimy patrzeć krytycznie. Niestety `capitalized` ma ograniczenie. Zawsze zamienia pierwszą literę każdego słowa na wielką, a niekoniecznie zawsze tego chcemy.

## Zobacz także: 
Zapoznaj się z oficjalną dokumentacją Apple na temat `String` ([link](https://developer.apple.com/documentation/swift/string)) i tekstem na StackOverflow na temat `capitalized` ([link](https://stackoverflow.com/questions/26306326/swift-string-to-uppercase)).
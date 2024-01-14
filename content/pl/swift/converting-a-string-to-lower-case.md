---
title:                "Swift: Konwersja ciągu znaków na małe litery"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu potrzebujemy zmienić wielkość liter w stringu na mniejszą. Może to być potrzebne do porównania tekstów lub do pobrania danych z zewnętrznych zgodnie z określonym formatem. W tym artykule dowiesz się, dlaczego konwertowanie stringu na mniejszą wielkość liter jest ważne i jak to zrobić w języku Swift.

## Jak to zrobić

Aby skonwertować string na mniejszą wielkość liter w Swift, możemy użyć metody `lowercased ()`. Przykłady poniżej pokazują, jak to zrobić w praktyce.

```
let originalString = "KONWERTOWANIE STRINGU NA MNIEJSZĄ WIELKOŚĆ LITER"
let lowercasedString = originalString.lowercased()

print(lowercasedString)
```

Wynik:
```
konwertowanie stringu na mniejszą wielkość liter
```

Możemy również wykorzystać tę metodę do porównywania dwóch stringów, niezależnie od tego, czy są one napisane z wykorzystaniem małych lub dużych liter.

```
let firstString = "Pies"
let secondString = "pies"

if firstString.lowercased() == secondString.lowercased() {
  print("Oba napisy są identyczne.")
} else {
  print("Napisy różnią się wielkością liter.")
}
```

Wynik:
```
Oba napisy są identyczne.
```

## Deep Dive

W języku Swift istnieje również metoda `uppercased ()`, która pozwala na zmianę wszystkich liter w stringu na wielkie. Warto również wspomnieć, że metody `lowercased ()` i `uppercased ()` są bezpieczne dla znaków Unicode, co oznacza, że ​​nie zmienią one znaków specjalnych, takich jak litery z akcentami.

Inną przydatną opcją jest użycie metody `capitalized ()`, która pozwala na zmianę pierwszej litery w każdym słowie stringu na wielką. Na przykład:

```
let originalString = "skonwertować string na mniejszą wielkość liter"
let capitalizedString = originalString.capitalized()

print(capitalizedString)
```

Wynik:
```
Skonwertować String Na Mniejszą Wielkość Liter
```

## Zobacz także

- Dokumentacja Apple o metodzie `lowercased ()`: https://developer.apple.com/documentation/swift/string/2427940-lowercased
- Wprowadzenie do języka Swift: https://developer.apple.com/swift/
- Inne przydatne metody związane ze stringami w Swift: https://learnappmaking.com/string-string-interpolation-formatting-swift-how-to/
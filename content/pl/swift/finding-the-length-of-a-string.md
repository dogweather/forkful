---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Swift: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości napisu może być niezbędne podczas tworzenia programów w języku Swift. Może być wykorzystywane do walidacji danych wejściowych lub do operacji na tekstach.

## Jak

```Swift
let name = "John"
let length = name.count
print("Długość imienia to \(length) znaków.")
```

W powyższym przykładzie tworzymy zmienną „name”, zawierającą imię „John”. Następnie używamy wbudowanej funkcji „count” na zmiennej „name”, aby uzyskać długość napisu. W końcu drukujemy wynik, używając interpolacji ciągu znaków. Ta sama metoda może być również użyta do sprawdzania długości innych napisów.

## Deep Dive

Długość napisu jest określana przez liczbę znaków, które składają się na dany napis. W języku Swift, każdy znak, niezależnie od tego czy jest literą czy symbolem, jest traktowany jako jedna jednostka. Dzięki temu funkcja „count” jest w stanie poprawnie obliczać długość napisu, nawet jeśli zawiera on różnego rodzaju znaki.

Ponadto, funkcja „count” jest zawsze dostosowana do aktualnego języka urządzenia użytkownika. Oznacza to, że jeśli użytkownik ma ustawiony język na polski, funkcja ta będzie zliczać polskie znaki jako jednostki.

Jeśli używasz starszych wersji języka Swift, musisz używać funkcji „characters.count” zamiast „count” do obliczania długości napisu.

## Zobacz także

- [Swift - String](https://developer.apple.com/documentation/swift/string)
- [Swift - Counting Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID285)
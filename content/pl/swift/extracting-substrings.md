---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wyodrębnianie podsłowo (substringów) to metoda rozdzielania dłuższych ciągów znaków na mniejsze segmenty, zgodnie z określonymi kryteriami. Robimy to, aby przetworzyć, przeanalizować lub manipulować pojedynczymi częściami większych zestawów danych.

## Jak to zrobić?
W Swift zazwyczaj wykorzystujemy metody `prefix()`, `suffix()`, `dropFirst()`, `dropLast()`, i zakresy indeksów do manipulowania podsłowami. Zobaczmy to w praktyce:

```Swift
let zdanie = "To jest przykładowe zdanie"

let prefix = zdanie.prefix(5) // "To je"
let suffix = zdanie.suffix(6) // " zdanie"
let przerwaneZdanie = zdanie.dropFirst(3) // "jest przykładowe zdanie"
let przycieteZdanie = zdanie.dropLast(7) // "To jest przykładowe"

let indexStart = zdanie.index(zdanie.startIndex, offsetBy: 3)
let indexEnd = zdanie.index(zdanie.endIndex, offsetBy: -7)
let podsłowo = zdanie[indexStart..<indexEnd] //"jest przykładowe"
```

## Dogłębna Analiza
Podsłowa w Swift zyskały na znaczeniu w Swift 4, gdy Apple wprowadził nowy typ `Substring`. Do tej pory programiści musieli radzić sobie z indeksacją String przez używanie pozycji Unicode, co było skomplikowane i niewygodne.

Co do alternatyw, programiści mogą również korzystać z metody `split(separator:)` lub `components(separatedBy:)` do wyodrębniania podsłów na podstawie zdefiniowanego separatora.

Kiedy używasz metody getType do uzyskania typu podciągu, zauważysz, że zwraca typ Substring, a nie String. To dlatego, że Substring w Swift jest odniesieniem do części ciągu znaków, nie jest to osobny obiekt.

## Zobacz Również
 - [Apple's String and Characters Guide](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
 - [Paul Hudson's Hacking with Swift: How to manipulate strings in Swift](https://www.hackingwithswift.com/articles/115/how-to-manipulate-strings-in-swift)
 - [Substring Documentation on Swift](https://developer.apple.com/documentation/swift/substring)
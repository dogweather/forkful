---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:50:16.233102-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Losowe liczby to dla kompa jak sól dla potrawy – dodają smaku. W programowaniu używasz ich do gier, symulacji, testów, no i tam gdzie chcesz odrobinę nieprzewidywalności.

## How to: (Jak to zrobić:)
```Swift
import Foundation

// Proste losowanie liczby
let randomNumber = Int.random(in: 1...100)
print(randomNumber) // np. 42

// Losowanie liczby zmiennoprzecinkowej
let randomFloat = Float.random(in: 0..<1)
print(randomFloat) // np. 0.84375

// Tworzenie losowej tablicy
var array = (1...10).map { _ in Int.random(in: 1...100) }
print(array) // np. [53, 92, 15, 35, 72, 65, 18, 90, 34, 23]
```
Mamy tutaj proste sposoby na losowe liczby całkowite, zmiennoprzecinkowe i losowe elementy w tablicy.

## Deep Dive (Dogłębna analiza)
Historia losowości w informatyce sięga daleko w XX wiek. Kiedyś używano pierwotnych generatorów, jak np. LCG (Linear Congruential Generator). W Swift mamy fajne API, które za nas załatwia te skomplikowane obliczenia.

Oprócz `random(in:)`, masz też inne opcje. Metoda `arc4random()` była popularna w Objective-C, ale w Swift też się ją czasem widzi. Problemy? Może być mniej bezpieczna dla kryptografii i trochę przestarzała.

Swift posługuje się protokołem `RandomNumberGenerator`. Możesz mieć swój generator liczb losowych, np. dla gier gdzie potrzebujesz bardziej precyzyjnej kontroli nad losowością.

## See Also (Zobacz również)
- [Dokumentacja Swift dla `random(in:)`](https://developer.apple.com/documentation/swift/int/2995648-random)
- [Opis protokołu `RandomNumberGenerator`](https://developer.apple.com/documentation/swift/randomnumbergenerator)
- [Książka „Swift Programming: The Big Nerd Ranch Guide”](https://www.bignerdranch.com/books/swift-programming/) dla bardziej szczegółowego zrozumienia Swift.
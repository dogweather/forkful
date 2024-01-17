---
title:                "Tworzenie liczb losowych"
html_title:           "Swift: Tworzenie liczb losowych"
simple_title:         "Tworzenie liczb losowych"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Czym jest generowanie losowych liczb i dlaczego programiści to robią?
Generowanie losowych liczb jest procesem tworzenia liczb bez określonego porządku. Programiści używają tego do różnych zastosowań, takich jak gry losowe, testowanie oprogramowania i szyfrowanie danych.

## Jak to zrobić:
```swift
//generowanie losowej liczby z przedziału 1-10
let randomNumber = Int.random(in: 1...10)
print(randomNumber) //możesz dostosować zakres i użyć Float.random lub Double.random dla liczb ułamkowych
//przykładowy wynik: 8
```

```swift
//generowanie losowego elementu z listy
let fruits = ["jabłko", "banan", "truskawka", "pomarańcza"]
let randomFruit = fruits.randomElement()
print(randomFruit) //przykładowy wynik: truskawka
```

## Głębsze nurkowanie:
Generowanie losowych liczb miało początki w matematyce, ale stało się popularne w programowaniu pojawieniem się komputerów. Istnieją również alternatywne metody generowania liczb losowych, takie jak generator liczb pseudolosowych, który wykorzystuje algorytmy do symulowania przypadkowego wyboru.

## Zobacz również:
Dowiedz się więcej o bibliotece Swift do losowych liczb [tutaj](https://developer.apple.com/documentation/swift/int/2995640-random) i sprawdź alternatywne podejścia [tutaj] (https://developer.apple.com/documentation/swift/int/2923548-arc4random).
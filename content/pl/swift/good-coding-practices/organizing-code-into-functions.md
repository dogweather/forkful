---
date: 2024-01-26 01:16:19.276639-07:00
description: "Jak to zrobi\u0107: Wyobra\u017A sobie zadanie: oblicz \u015Bredni\u0105\
  \ z tablicy. Bez funkcji, wrzuci\u0142by\u015B to wszystko do funkcji main. Z funkcjami\
  \ zrobi\u0142by\u015B to tak."
lastmod: '2024-03-13T22:44:35.760426-06:00'
model: gpt-4-0125-preview
summary: "Wyobra\u017A sobie zadanie."
title: Organizowanie kodu w funkcje
weight: 18
---

## Jak to zrobić:
Wyobraź sobie zadanie: oblicz średnią z tablicy. Bez funkcji, wrzuciłbyś to wszystko do funkcji main. Z funkcjami zrobiłbyś to tak:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// Sposób użycia
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("Średni wynik to \(averageScore)")
```

Przykładowy wynik to: 
```
Średni wynik to 87.6875
```

## Szczegółowa analiza
Historycznie, w miarę komplikowania się programowania, funkcje stały się kamieniem węgielnym zarządzania złożonością. Alternatywy obejmują pisanie kodu w jednym ciągu i kopiowanie-wklejanie kodu (kod spaghetti) – obecnie są one w dużej mierze uważane za złą praktykę. W Swift, funkcje są obywatelami pierwszej klasy; mogą być przypisywane do zmiennych, przekazywane jako argumenty i zwracane z innych funkcji, co sprawia, że kod jest bardziej modułowy i elastyczny.

Jeśli chodzi o implementację, zaprojektuj swoje funkcje, by dobrze wykonywały jedno zadanie. Dąż do funkcji o jasnym celu i nazwie, która go odzwierciedla. Zwracaj uwagę na liczbę parametrów - jeśli jest ich za dużo, prawdopodobnie robisz za dużo. Co z obsługą błędów? Rozważ funkcje rzucające i łagodne radzenie sobie z problemami. Pamiętaj: w Swifcie chodzi o czytelność i łatwość utrzymania.

## Zobacz również
- [Przewodnik po języku programowania Swift - Funkcje](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Przewodnik po stylu Swifta autorstwa Raya Wenderlicha](https://github.com/raywenderlich/swift-style-guide)
- [Refaktoryzacja: Usprawnianie projektu istniejącego kodu Martina Fowlera](https://martinfowler.com/books/refactoring.html)

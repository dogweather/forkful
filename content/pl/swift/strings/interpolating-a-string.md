---
date: 2024-01-20 17:52:00.488180-07:00
description: "Interpolacja string\xF3w to wstawienie warto\u015Bci zmiennych lub wyra\u017C\
  e\u0144 wewn\u0105trz stringu. Robimy to, by \u0142atwo tworzy\u0107 personalizowane\
  \ wiadomo\u015Bci i dynamicznie\u2026"
lastmod: '2024-03-13T22:44:35.741042-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja string\xF3w to wstawienie warto\u015Bci zmiennych lub wyra\u017C\
  e\u0144 wewn\u0105trz stringu."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## Jak to zrobić:
```Swift
// Prosta interpolacja
let name = "Marek"
let greeting = "Cześć, \(name)!"
print(greeting) // Wyjście: Cześć, Marek!

// Zawansowana interpolacja z operacjami matematycznymi
let apples = 3
let oranges = 5
let fruitSummary = "Mam \(apples + oranges) owoców."
print(fruitSummary) // Wyjście: Mam 8 owoców.

// Interpolacja z wyrażeniami warunkowymi
let temperature = 18
let weatherMessage = "Dzisiaj jest \(temperature > 15 ? "ciepło" : "zimno")."
print(weatherMessage) // Wyjście: Dzisiaj jest ciepło.
```

## Zagłębienie się
Interpolacja stringów w Swift została wprowadzona razem z pierwszą wersją języka w 2014 roku, jako część jego flexybilności. Przed tym, w językach jak C, budowanie stringów wymagało funkcji typu `sprintf`, a w rozwiązaniach takich jak .NET czy Java stosowano konkatenację stringów lub klasy pomocnicze.

Oprócz interpolacji bezpośredniej, Swift oferuje możliwość bardziej zaawansowanej kontroli nad formatowaniem. Można to robić za pomocą specjalnych konstruktorów String, takich jak `String(format:)`, które działają podobnie do `printf` w języku C.

Swift pozwala nawet na definiowanie własnych ekspresji interpolujących, dzięki czemu możesz dostosowywać, formatować i wyświetlać dane w praktycznie dowolny sposób, lecz w tym artykule skupimy się na podstawach.

## Zobacz również
- Oficjalna dokumentacja Swift na temat interpolacji stringów: [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- Doskonałe podsumowanie o interpolacji stringów w Swift: [Swift String Interpolation](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5-0)
